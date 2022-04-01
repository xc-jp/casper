{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Content
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
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.String (fromString)
import qualified Data.Text.Encoding as Text
import Data.Typeable
import qualified Data.UUID as UUID
import GHC.Conc (unsafeIOToSTM)
import GHC.Generics
import LargeWords (Word128 (..), Word256 (..))
import Ref
import Rescope
import Var

newtype TransactionID = TransactionID Word

data Cache = Cache
  { resourceCache :: TVar (DMap UUID),
    contentCache :: TVar (DMap SHA),
    resourceUsers :: UserTracker UUID, -- determines whether a resource can be freed from memory
    contentUsers :: UserTracker SHA -- determines whether content can be freed from memory
  }

newtype Store = Store {storeCache :: Cache}

-- data Store (root :: * -> *) = Store
--   { storeDir :: FilePath,
--     storeCache :: Cache,
--     storeRoot :: UUID -- Var (root Var Ref)
--   }

newtype WrapAeson a = WrapAeson {unWrapAeson :: a}

instance (FromJSON a, ToJSON a) => Serialize (WrapAeson a) where
  put = Serialize.put . Aeson.encode . unWrapAeson
  get = Serialize.get >>= either fail (pure . WrapAeson) . Aeson.eitherDecodeStrict'

data TransactionCommits = TransactionCommits
  { varCommits :: HashMap UUID WritePair,
    refCommits :: HashMap SHA WritePair
  }

data WritePair = WritePair {writeTemp :: IO FilePath, moveTemp :: FilePath -> IO ()}

writeBracket :: WritePair -> IO () -> IO ()
writeBracket (WritePair write move) = Exception.bracket write move . const

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

newtype CasperT s m a = CasperT (ReaderT Store m a)
  deriving (Functor, Monad, Applicative, MonadIO)

-- loadStore :: FilePath -> (forall s. Var s (root s) -> CasperT s m a) -> m a
loadStore :: FilePath -> (forall s. root s -> CasperT s m a) -> m a
loadStore _ _ = undefined

-- TODO don't use/expose
getStore :: Applicative m => CasperT s m Store
getStore = CasperT $ ReaderT pure

runCasperT :: Store -> CasperT s m a -> m a
runCasperT store (CasperT (ReaderT k)) = k store

liftSTM :: STM a -> Transaction s a
liftSTM = Transaction . lift . lift

-- NOTE: this should be called with Var a s or Ref a s, otherwise the
-- datatype itself won't get pinned, only its nested content.
-- TODO: I don't think that's true
borrow ::
  (MonadIO m, MonadMask m, Content (f s), forall x. Rescope (f s) (f x)) =>
  Transaction s (f s) ->
  (forall t. f t -> CasperT t m a) ->
  CasperT s m a
borrow transaction k = do
  fs <- transact transaction
  pinRefs fs $ do
    k (rescope fs)

-- TODO These probably shouldn't be pinned as resources but as roots
pinRefs :: forall s f x m a. (MonadIO m, MonadMask m) => Content (f s) => f s -> CasperT x m a -> CasperT s m a
pinRefs fs (CasperT (ReaderT k)) =
  let (uuids, shas) = mconcat $ refs (\(Var a) -> ([unDKey a], [])) (\(Ref b) -> ([], [unDKey b])) fs
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
          bracket pin (const unpin) $ const $ k (coerce store)

transact ::
  (MonadIO m, MonadMask m) =>
  Transaction s a ->
  CasperT s m a
transact transaction = CasperT . ReaderT $ \(Store cache) -> do
  bracket pin (unpin cache) $ \(rLockSet', cLockSet') -> do
    (a, TransactionCommits ref var) <-
      let ctx = TransactionContext rLockSet' cLockSet' cache
       in liftIO . atomically . runTransaction ctx $ transaction
    liftIO $ foldr writeBracket (foldr writeBracket (pure ()) ref) var
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
    runTransaction ctx (Transaction m) = runReaderT (runStateT m (TransactionCommits mempty mempty)) ctx

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
    let UUID uuid = unDKey $ unVar ref
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

writeVar :: (Content a, Serialize a) => Var a s -> a -> Transaction s ()
writeVar var a = do
  tvar <- getVar var $ newTVarIO a
  liftSTM $ writeTVar tvar a -- This is unfortunately a double write if we create a new TVar
  let writeTemp = undefined
      moveTemp = undefined
  Transaction $ modify $ \(TransactionCommits v r) -> TransactionCommits (HashMap.insert (varUuid var) (WritePair writeTemp moveTemp) v) r

writeVarPair :: Var s a -> WritePair
writeVarPair = undefined

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
