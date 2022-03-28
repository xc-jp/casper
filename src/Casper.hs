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
  ( -- * transactions
    Transaction,
    transact,
    borrow,

    -- * Variables for mutable content
    Var,
    fakeVar,
    readVar,
    writeVar,
    newVar,

    -- * References to immutable content
    Ref,
    fakeRef,
    readRef,
    newRef,

    -- * CasperT and Store
    CasperT,
    Store,
    loadStore,
    getStore,
    runCasperT,
    liftSTM,

    -- * Type classes and data types
    Content (..),
    Rescope (..),
    WrapAeson (..),
  )
where

import Control.Applicative (liftA2)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.Catch (MonadMask, bracket)
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

newtype Ref a s = Ref (DKey SHA a)

fakeRef :: Ref a s
fakeRef = Ref $ unsafeMkDKey $ SHA (Word256 0 0 0 0)

instance Serialize SHA where
  get = SHA <$> (Word256 <$> Serialize.get <*> Serialize.get <*> Serialize.get <*> Serialize.get)
  put (SHA (Word256 a b c d)) = Serialize.put a <> Serialize.put b <> Serialize.put c <> Serialize.put d

instance ToJSON (Ref a s) where
  toJSON ref =
    Aeson.String $
      Text.decodeLatin1 $
        Base64.encode $
          Serialize.runPut $
            Serialize.put ref

instance FromJSON (Ref a s) where
  parseJSON v = do
    txt <- Aeson.parseJSON v
    let digest = Text.encodeUtf8 txt
    case Base64.decode digest of
      Left err -> fail err
      Right bytes -> case Serialize.runGet Serialize.get bytes of
        Left err -> fail err
        Right x -> pure x

instance Serialize (Ref a s) where
  get = Ref . unsafeMkDKey <$> Serialize.get
  put (Ref key) = Serialize.put $ unDKey key

instance Content s (Ref a s) where refs _ fc c = pure $ fc c

newtype Var a s = Var {unResourceRef :: DKey UUID (TVar a)}

fakeVar :: Var a s
fakeVar = Var $ unsafeMkDKey $ UUID $ Word128 0 0

instance Serialize (Var a s) where
  put (Var key) = Serialize.put $ unDKey key
  get = Var . unsafeMkDKey <$> Serialize.get

instance FromJSON (Var a s) where
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

instance ToJSON (Var a s) where
  toJSON (Var key) =
    let UUID w = unDKey key
     in Aeson.String (UUID.toText (toUUID w))

instance Content s (Var a s) where refs fr _ r = pure $ fr r

instance Content s a => Content s [a]

instance Content s Int where refs _ _ _ = []

class Content s a where
  refs ::
    (forall r. Var r s -> ref) ->
    (forall r. Ref r s -> ref) ->
    (a -> [ref])
  default refs ::
    (Generic a, GContent s (Rep a)) =>
    (forall r. Var r s -> ref) ->
    (forall r. Ref r s -> ref) ->
    (a -> [ref])
  refs fr fc a = grefs fr fc (from a)

class GContent s a where
  grefs ::
    (forall r. Var r s -> ref) ->
    (forall r. Ref r s -> ref) ->
    (a x -> [ref])

instance Content s a => GContent s (K1 c a) where grefs fr fc (K1 a) = refs fr fc a

instance GContent s a => GContent s (M1 i c a) where grefs fr fc (M1 a) = grefs fr fc a

instance (GContent s a, GContent s b) => GContent s (a :*: b) where grefs fr fc (a :*: b) = grefs fr fc a <> grefs fr fc b

instance (GContent s a, GContent s b) => GContent s (a :+: b) where
  grefs fr fc (L1 a) = grefs fr fc a
  grefs fr fc (R1 b) = grefs fr fc b

instance GContent s U1 where grefs _ _ _ = []

instance GContent s V1 where grefs _ _ _ = []

class Rescope a b where
  rescope :: a -> b
  default rescope :: (Generic a, Generic b, GRescope (Rep a) (Rep b)) => a -> b
  rescope a = to $ grescope (from a)

instance Rescope (Ref a s) (Ref a x) where
  rescope (Ref a) = Ref a

instance Rescope (Var a s) (Var a x) where
  rescope (Var a) = Var a

instance Rescope (Store s) (Store x) where
  rescope (Store cache) = Store cache

class GRescope a b where
  grescope :: a x -> b x

instance Rescope a b => GRescope (K1 c a) (K1 c' b) where grescope (K1 a) = K1 $ rescope a

instance GRescope a b => GRescope (M1 i c a) (M1 i c' b) where grescope (M1 a) = M1 $ grescope a

instance (GRescope a c, GRescope b d) => GRescope (a :*: b) (c :*: d) where grescope (a :*: b) = grescope a :*: grescope b

instance (GRescope a c, GRescope b d) => GRescope (a :+: b) (c :+: d) where
  grescope (L1 a) = L1 (grescope a)
  grescope (R1 b) = R1 (grescope b)

instance GRescope U1 U1 where
  grescope = id

instance GRescope V1 V1 where
  grescope = id

data Cache = Cache
  { resourceCache :: TVar (DMap UUID),
    contentCache :: TVar (DMap SHA),
    resourceUsers :: UserTracker UUID, -- determines whether a resource can be released
    contentUsers :: UserTracker SHA -- determines whether content can be released
  }

newtype Store s = Store {storeCache :: Cache}

newtype WrapAeson a = WrapAeson {unWrapAeson :: a}

instance (FromJSON a, ToJSON a) => Serialize (WrapAeson a) where
  put = Serialize.put . Aeson.encode . unWrapAeson
  get = Serialize.get >>= either fail (pure . WrapAeson) . Aeson.eitherDecodeStrict'

-- TODO only write once per SHA/UUID
-- TODO Make atomic write/move pairs
newtype TransactionCommits = TransactionCommits {onCommit :: IO ()}

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

newtype CasperT s m a = CasperT (ReaderT (Store s) m a)
  deriving (Functor, Monad, Applicative, MonadIO)

loadStore :: FilePath -> (forall s. Var s (root s) -> CasperT s m a) -> m a
loadStore _ _ = undefined

getStore :: Applicative m => CasperT s m (Store s)
getStore = CasperT $ ReaderT pure

runCasperT :: Store s -> CasperT s m a -> m a
runCasperT store (CasperT (ReaderT k)) = k store

liftSTM :: STM a -> Transaction s a
liftSTM = Transaction . lift . lift

-- NOTE: this should be called with Var a s or Ref a s, otherwise the
-- datatype itself won't get pinned, only its nested content.
borrow ::
  (MonadIO m, MonadMask m, Content s (f s), forall x. Rescope (f s) (f x)) =>
  Transaction s (f s) ->
  (forall x. f x -> CasperT x m a) ->
  CasperT s m a
borrow transaction k = do
  fs <- transact transaction
  pinRefs fs $ do
    k (rescope fs)

pinRefs :: forall s f x m a. (MonadIO m, MonadMask m) => Content s (f s) => f s -> CasperT x m a -> CasperT s m a
pinRefs fs (CasperT (ReaderT k)) =
  let (uuids, shas) = mconcat $ refs @s (\(Var a) -> ([unDKey a], [])) (\(Ref b) -> ([], [unDKey b])) fs
   in CasperT $
        ReaderT $ \store -> do
          let cache = storeCache store
          let pin = liftIO $
                atomically $ do
                  forM_ uuids $ \uuid -> bump uuid (resourceUsers cache)
                  forM_ shas $ \sha -> bump sha (contentUsers cache)
          let unpin = liftIO $
                atomically $ do
                  forM_ uuids $ \uuid -> debump uuid (resourceUsers cache)
                  forM_ shas $ \sha -> debump sha (contentUsers cache)
          bracket pin (const unpin) $ const $ k (rescope store)

transact ::
  (MonadIO m, MonadMask m) =>
  Transaction s a ->
  CasperT s m a
transact transaction = CasperT . ReaderT $ \(Store cache) -> do
  bracket pin (unpin cache) $ \(rLockSet', cLockSet') -> do
    (a, TransactionCommits commits) <-
      let ctx = TransactionContext rLockSet' cLockSet' cache
       in liftIO . atomically . runTransaction ctx $ transaction
    liftIO commits
    pure a
  where
    pin = liftIO $ do
      liftA2 (,) (newTVarIO mempty) (newTVarIO mempty)
    unpin cache (rLockSet', cLockSet') = liftIO $ do
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

getVar :: Var a s -> IO (TVar a) -> Transaction s (TVar a)
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

readVar :: Serialize a => Var a s -> Transaction s a
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

writeVar :: (Content s a, Serialize a) => Var a s -> a -> Transaction s ()
writeVar ref a = do
  var <- getVar ref $ newTVarIO a
  liftSTM $ writeTVar var a -- This is unfortunately a double write if we create a new TVar
  -- TODO add writeback action to commits

newVar :: a -> Transaction s (Var a s)
newVar = undefined

readRef :: Ref a s -> Transaction s a
readRef = undefined

newRef :: a -> Transaction s (Ref a s)
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
