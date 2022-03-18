{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Casper
  ( transact,
    readVar,
    writeVar,
    newVar,
    readRef,
    newRef,
    loadStore,
    liftSTM,
    WrapAeson (..),
    Content (..),
    Transaction,
    CasperT,
    Var,
    fakeVar,
    Ref,
    fakeRef,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader
import Control.Monad.State
import DMap
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as Base64
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.Text.Encoding as Text
import Data.Typeable
import qualified Data.UUID as UUID
import GHC.Conc (unsafeIOToSTM)
import GHC.Generics
import LargeWords (Word128 (..), Word256 (..))

newtype TransactionID = TransactionID Word

newtype UUID = UUID Word128
  deriving newtype (Eq, Hashable)

instance Serialize UUID where
  put (UUID (Word128 a b)) = Serialize.put a <> Serialize.put b
  get = UUID <$> (Word128 <$> Serialize.get <*> Serialize.get)

newtype SHA = SHA Word256
  deriving newtype (Eq, Hashable)

newtype Ref s a = Ref (DKey SHA a)

fakeRef :: Ref s a
fakeRef = Ref $ unsafeMkDKey $ SHA (Word256 0 0 0 0)

instance Serialize SHA where
  get = SHA <$> (Word256 <$> Serialize.get <*> Serialize.get <*> Serialize.get <*> Serialize.get)
  put (SHA (Word256 a b c d)) = Serialize.put a <> Serialize.put b <> Serialize.put c <> Serialize.put d

instance ToJSON (Ref s a) where
  toJSON ref =
    Aeson.String $
      Text.decodeLatin1 $
        Base64.encode $
          Serialize.runPut $
            Serialize.put ref

instance FromJSON (Ref s a) where
  parseJSON v = do
    txt <- Aeson.parseJSON v
    let digest = Text.encodeUtf8 txt
    case Base64.decode digest of
      Left err -> fail err
      Right bytes -> case Serialize.runGet Serialize.get bytes of
        Left err -> fail err
        Right x -> pure x

instance Serialize (Ref s a) where
  get = Ref . unsafeMkDKey <$> Serialize.get
  put (Ref key) = Serialize.put $ unDKey key

instance Content s (Ref s a) where refs _ fc c = pure $ fc c

newtype Var s a = Var {unResourceRef :: DKey UUID (TVar a)}

fakeVar :: Var s a
fakeVar = Var $ unsafeMkDKey $ UUID $ Word128 0 0

instance Serialize (Var s a) where
  put (Var key) = Serialize.put $ unDKey key
  get = Var . unsafeMkDKey <$> Serialize.get

-- put (Var (Word128 a b)) = UUID.toStrict

instance FromJSON (Var s a) where
  parseJSON v = do
    txt <- Aeson.parseJSON v
    case UUID.fromText txt of
      Nothing -> fail (show txt <> " isn't a valid UUID")
      Just a -> pure $ Var $ unsafeMkDKey $ UUID (fromUUID a)

toUUID :: Word128 -> UUID.UUID
toUUID (Word128 a b) = UUID.fromWords64 a b

fromUUID :: UUID.UUID -> Word128
fromUUID u = Word128 a b
  where
    (a, b) = UUID.toWords64 u

instance ToJSON (Var s a) where
  toJSON (Var key) =
    let UUID w = unDKey key
     in Aeson.String (UUID.toText (toUUID w))

instance Content s (Var s a) where refs fr _ r = pure $ fr r

instance Content s a => Content s [a]

instance Content s Int where refs _ _ _ = []

class Serialize a => Content s a where
  refs ::
    (forall r. Var s r -> ref) ->
    (forall r. Ref s r -> ref) ->
    (a -> [ref])
  default refs ::
    (Generic a, GContent s (Rep a)) =>
    (forall r. Var s r -> ref) ->
    (forall r. Ref s r -> ref) ->
    (a -> [ref])
  refs fr fc a = grefs fr fc (from a)

class GContent s a where
  grefs ::
    (forall r. Var s r -> ref) ->
    (forall r. Ref s r -> ref) ->
    (a x -> [ref])

instance Content s a => GContent s (K1 c a) where grefs fr fc (K1 a) = refs fr fc a

instance GContent s a => GContent s (M1 i c a) where grefs fr fc (M1 a) = grefs fr fc a

instance (GContent s a, GContent s b) => GContent s (a :*: b) where grefs fr fc (a :*: b) = grefs fr fc a <> grefs fr fc b

instance (GContent s a, GContent s b) => GContent s (a :+: b) where
  grefs fr fc (L1 a) = grefs fr fc a
  grefs fr fc (R1 b) = grefs fr fc b

instance GContent s U1 where grefs _ _ _ = []

instance GContent s V1 where grefs _ _ _ = []

data Cache = Cache
  { resourceCache :: TVar (DMap UUID),
    contentCache :: TVar (DMap SHA),
    resourceUsers :: UserTracker UUID, -- determines whether a resource can be released
    contentUsers :: UserTracker SHA -- determines whether content can be released
  }

data Store (root :: * -> *) = Store
  { storeCache :: Cache,
    storeRoot :: UUID -- Var (root Var Ref)
  }

newtype WrapAeson a = WrapAeson {unWrapAeson :: a}

instance (FromJSON a, ToJSON a) => Serialize (WrapAeson a) where
  put = Serialize.put . Aeson.encode . unWrapAeson
  get = Serialize.get >>= either fail (pure . WrapAeson) . Aeson.eitherDecodeStrict'

keepAlive :: [UUID] -> CasperT s root m a -> CasperT s root m a
keepAlive = undefined

-- TODO only write once per SHA/UUID
-- TODO Make atomic write/move pairs
newtype TransactionCommits = TransactionCommits {onCommit :: IO ()}

-- newtype Datasets mut imm = Datasets (Map UUID (mut (Dataset mut imm)))

data TransactionContext = TransactionContext
  { txResourceLocks :: TVar (HashSet UUID),
    txContentLocks :: TVar (HashSet SHA),
    txCache :: Cache
  }

newtype Transaction s a = Transaction
  { unTransaction ::
      StateT
        TransactionCommits
        (ReaderT TransactionContext STM)
        a
  }
  deriving (Functor, Monad, Applicative)

newtype CasperT s root m a = CasperT (ReaderT (Store root) m a)
  deriving (Functor, Monad, Applicative, MonadIO)

loadStore :: FilePath -> (forall s. CasperT s root m a) -> m a
loadStore _ _ = undefined

liftSTM :: STM a -> Transaction s a
liftSTM = Transaction . lift . lift

transact ::
  forall root q a.
  (forall s. Serialize (root s)) =>
  -- TODO hide Var and Ref in this constraint
  (forall s. root s -> Transaction s a) ->
  CasperT q root IO a
transact action = CasperT . ReaderT $ \(Store cache rootRef) -> do
  rLockSet' <- newTVarIO mempty
  cLockSet' <- newTVarIO mempty
  let ctx = TransactionContext rLockSet' cLockSet' cache
  (a, TransactionCommits commits) <-
    atomically . runTransaction ctx $ do
      root <- readVar (Var $ unsafeMkDKey rootRef)
      action root

  commits
  rLockSet <- readTVarIO rLockSet'
  forM_ rLockSet $ \rLock -> atomically $ do
    count <- debump rLock (resourceUsers cache)
    when (count < 1) $ do
      modifyTVar (resourceCache cache) (DMap.delete' rLock)
  cLockSet <- readTVarIO cLockSet'
  forM_ cLockSet $ \cLock -> atomically $ do
    count <- debump cLock (contentUsers cache)
    when (count < 1) $ do
      modifyTVar (contentCache cache) (DMap.delete' cLock)
  -- TODO collect garbage
  pure a
  where
    runTransaction ::
      TransactionContext ->
      Transaction s a ->
      STM (a, TransactionCommits)
    runTransaction ctx (Transaction m) = runReaderT (runStateT m (TransactionCommits (pure ()))) ctx

newtype UserTracker k = UserTracker (TVar (HashMap k (TVar Int)))

bump :: (Eq k, Hashable k) => k -> UserTracker k -> STM ()
bump k (UserTracker userMap') = do
  userMap <- readTVar userMap'
  case HashMap.lookup k userMap of
    Nothing -> do
      count' <- newTVar 1
      modifyTVar userMap' (HashMap.insert k count')
    Just count' -> modifyTVar count' succ

debump :: (Eq k, Hashable k) => k -> UserTracker k -> STM Int
debump k (UserTracker userMap') = do
  userMap <- readTVar userMap'
  case HashMap.lookup k userMap of
    Nothing -> throwSTM (Exception.AssertionFailed "can't debump non-existent key")
    Just count' -> do
      count <- readTVar count'
      let newCount = count - 1
      if newCount < 1
        then modifyTVar userMap' (HashMap.delete k)
        else writeTVar count' newCount
      pure newCount

checkUsers :: (Eq k, Hashable k) => k -> UserTracker k -> STM Int
checkUsers k (UserTracker userMap') = do
  userMap <- readTVar userMap'
  case HashMap.lookup k userMap of
    Nothing -> pure 0
    Just count' -> readTVar count'

getVar :: Var s a -> IO (TVar a) -> Transaction s (TVar a)
getVar ref createNewVar = Transaction $ do
  TransactionContext resourceLocks _ (Cache rcache _ rusers _) <- ask
  let Var key = ref
      uuid = unDKey key
  lift . lift . safeIOToSTM $ do
    atomically $ do
      hasLock <- HashSet.member uuid <$> readTVar resourceLocks
      unless hasLock $ do
        modifyTVar resourceLocks (HashSet.insert uuid)
        bump uuid rusers
    mCachedVar <- DMap.lookup key <$> readTVarIO rcache
    case mCachedVar of
      Nothing -> do
        var <- createNewVar
        -- Just in case the resource was inserted while we were reading/decoding it,
        -- we recheck the cache before inserting
        atomically $ do
          mCachedVar' <- DMap.lookup key <$> readTVar rcache
          case mCachedVar' of
            Nothing -> do
              modifyTVar rcache (DMap.insert var key)
              pure var
            Just cachedVar -> pure cachedVar
      Just cachedVar -> pure cachedVar

readVar :: Serialize a => Var s a -> Transaction s a
readVar ref = do
  var <- getVar ref $ do
    let UUID uuid = unDKey $ unResourceRef ref
    bs <- readVarFromDisk (UUID uuid)
    case Serialize.decode bs of
      Left err ->
        error $
          unwords
            [ "corrupt content for",
              show (toUUID uuid) <> ":",
              err
            ]
      Right !a -> newTVarIO a
  liftSTM (readTVar var)

writeVar :: Content s a => Var s a -> a -> Transaction s ()
writeVar ref a = do
  var <- getVar ref $ newTVarIO a
  liftSTM $ writeTVar var a -- This is unfortunately a double write if we create a new TVar
  -- TODO add writeback action to commits

newVar :: a -> Transaction s (Var s a)
newVar = undefined

readRef :: Ref s a -> Transaction s a
readRef = undefined

newRef :: a -> Transaction s (Ref s a)
newRef = undefined

-- | Assures that the IO computation finalizes no matter if the STM transaction
-- is aborted or retried. The IO computation run in a different thread.
-- The STM transaction wait until the completion of the IO procedure (or retry as usual).
--
-- It can be retried if the embedding STM computation is retried
-- so the IO computation must be idempotent.
-- Exceptions are bubbled up to the STM transaction
--
-- copied from TCache
safeIOToSTM :: IO a -> STM a
safeIOToSTM req = unsafeIOToSTM $ do
  tv <- newEmptyMVar
  void . forkIO $
    Exception.catch
      (req >>= putMVar tv . Right)
      (\(e :: Exception.SomeException) -> putMVar tv $ Left e)
  r <- takeMVar tv
  case r of
    Right x -> return x
    Left e -> Exception.throw e

readVarFromDisk :: UUID -> IO ByteString
readVarFromDisk = undefined

writeVarToDisk :: UUID -> ByteString -> IO ()
writeVarToDisk = undefined
