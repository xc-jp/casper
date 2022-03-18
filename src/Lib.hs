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

-- TODO
--   Var
--   Ref

module Lib
  ( transact,
    -- localRoot,
    readMut,
    writeMut,
    newMut,
    loadStore,
    liftSTM,
    WrapAeson (..),
    Content (..),
    Transaction,
    CasperT,
    RRef,
    CRef,
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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.Typeable
import GHC.Conc (unsafeIOToSTM)
import GHC.Generics
import LargeWords

newtype TransactionID = TransactionID Word

newtype UUID = UUID Word128
  deriving newtype (Eq, Hashable)

newtype SHA = SHA Word256
  deriving newtype (Eq, Hashable)

newtype CRef s a = CRef (DKey SHA a)

instance FromJSON (CRef s a) where parseJSON = undefined

instance ToJSON (CRef s a) where toJSON = undefined

instance Serialize (CRef s a) where
  get = undefined
  put = undefined

instance Content s (CRef s a) where refs _ fc c = pure $ fc c

newtype RRef s a = RRef {unResourceRef :: DKey UUID (TVar a)}

instance Serialize (RRef s a) where
  get = undefined
  put = undefined

instance FromJSON (RRef s a) where parseJSON = undefined

instance ToJSON (RRef s a) where toJSON = undefined

instance Content s (RRef s a) where refs fr _ r = pure $ fr r

instance Content s a => Content s [a]

instance Content s Int where refs _ _ _ = []

class Serialize a => Content s a where
  refs ::
    (forall r. RRef s r -> ref) ->
    (forall r. CRef s r -> ref) ->
    (a -> [ref])
  default refs ::
    (Generic a, GContent s (Rep a)) =>
    (forall r. RRef s r -> ref) ->
    (forall r. CRef s r -> ref) ->
    (a -> [ref])
  refs fr fc a = grefs fr fc (from a)

class GContent s a where
  grefs ::
    (forall r. RRef s r -> ref) ->
    (forall r. CRef s r -> ref) ->
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
    storeRoot :: UUID -- RRef (root RRef CRef)
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
  -- TODO hide RRef and CRef in this constraint
  (forall s. root s -> Transaction s a) ->
  CasperT q root IO a
transact action = CasperT . ReaderT $ \(Store cache rootRef) -> do
  rLockSet' <- newTVarIO mempty
  cLockSet' <- newTVarIO mempty
  let ctx = TransactionContext rLockSet' cLockSet' cache
  (a, TransactionCommits commits) <-
    atomically . runTransaction ctx $ do
      root <- readMut (RRef $ unsafeMkDKey rootRef)
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

-- transact' ::
--   -- TODO hide RRef and CRef in this constraint
--   Content RRef CRef (root RRef CRef) =>
--   ( forall mut imm.
--     localRoot mut imm ->
--     Transaction mut imm a
--   ) ->
--   CasperT' s storeRoot localRoot IO a

-- newtype CasperT' s storeRoot localRoot m a
--   = CasperT'
--       ( ReaderT
--           (forall m i. storeRoot m i -> Transaction m i (m (localRoot m i)))
--           (CasperT s storeRoot m)
--           a
--       )

{- FIXME
localRoot ::
  ( forall mut imm.
    root mut imm ->
    Transaction mut imm (mut (root' mut imm))
  ) ->
  CasperT s root' m a ->
  CasperT s root m a
localRoot _ = undefined
-}

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
    Nothing -> error "asdfasdf"
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

getMutVar :: RRef s a -> IO (TVar a) -> Transaction s (TVar a)
getMutVar ref createNewVar = Transaction $ do
  TransactionContext resourceLocks _ (Cache rcache _ rusers _) <- ask
  let RRef key = ref
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
        newVar <- createNewVar
        -- Just in case the resource was inserted while we were reading/decoding it,
        -- we recheck the cache before inserting
        atomically $ do
          mCachedVar' <- DMap.lookup key <$> readTVar rcache
          case mCachedVar' of
            Nothing -> do
              modifyTVar rcache (DMap.insert newVar key)
              pure newVar
            Just cachedVar -> pure cachedVar
      Just cachedVar -> pure cachedVar

readMut :: Serialize a => RRef s a -> Transaction s a
readMut ref = do
  var <- getMutVar ref $ do
    let uuid = unDKey $ unResourceRef ref
    bs <- readImmFromDisk uuid
    case Serialize.decode bs of
      Left err -> error err
      Right !a -> newTVarIO a
  liftSTM (readTVar var)

parseContentRef :: ByteString -> Either String (CRef s r)
parseContentRef = error "not implemented"

parseResourceRef :: ByteString -> Either String (RRef s r)
parseResourceRef = error "not implemented"

writeMut :: Content s a => RRef s a -> a -> Transaction s ()
writeMut ref a = do
  var <- getMutVar ref $ newTVarIO a
  liftSTM $ writeTVar var a -- This is unfortunately a double write if we create a new TVar
  -- TODO add writeback action to commits

newMut :: a -> Transaction s (RRef s a)
newMut = undefined

readImm :: CRef s a -> Transaction s a
readImm = undefined

newImm :: a -> Transaction s (CRef s a)
newImm = undefined

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

readImmFromDisk :: UUID -> IO ByteString
readImmFromDisk dkey = undefined

writeImmToDisk :: UUID -> ByteString -> IO ()
writeImmToDisk = undefined
