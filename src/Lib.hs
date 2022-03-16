{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib
  ( transact,
    -- localRoot,
    readMut,
    toUUID,
    writeMut,
    newMut,
    loadStore,
    liftSTM,
    Transaction,
    CasperT,
    UUID (..),
    SHA (..),
  )
where

import Content
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
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Typeable
import GHC.Conc (unsafeIOToSTM)
import GHC.Generics
import LargeWords

newtype TransactionID = TransactionID Word

newtype UUID = UUID Word128
  deriving newtype (Eq, Hashable)

newtype SHA = SHA Word256
  deriving newtype (Eq, Hashable)

newtype ContentRef a = ContentRef (DKey SHA a)

newtype ResourceRef a = ResourceRef {unResourceRef :: DKey UUID (TVar a)}

data Cache = Cache
  { resourceCache :: TVar (DMap UUID),
    contentCache :: TVar (DMap SHA),
    resourceUsers :: UserTracker UUID, -- determines whether a resource can be released
    contentUsers :: UserTracker SHA -- determines whether content can be released
  }

data Store root = Store
  { storeCache :: Cache,
    storeRoot :: ResourceRef (root ResourceRef ContentRef)
  }

keepAlive :: [UUID] -> CasperT s root m a -> CasperT s root m a
keepAlive = undefined

{-

set1     |--| |--| |--| |--| |--| |--| |--|
datasets |--| |--| |--| |--| |--| |--| |--|
root |--------------------------------------|

-}

-- TODO only write once per SHA/UUID
-- TODO Make atomic write/move pairs
newtype TransactionCommits = TransactionCommits {onCommit :: IO ()}

-- newtype Datasets mut imm = Datasets (Map UUID (mut (Dataset mut imm)))

data TransactionContext mut imm = TransactionContext
  { txResourceLocks :: TVar (HashSet UUID),
    txContentLocks :: TVar (HashSet SHA),
    txCache :: Cache,
    -- :(
    txMutRef :: forall a. mut a -> ResourceRef a,
    txRefMut :: forall a. ResourceRef a -> mut a,
    txImmRef :: forall a. imm a -> ContentRef a,
    txRefImm :: forall a. ContentRef a -> imm a
  }

newtype Transaction (mut :: Type -> Type) (imm :: Type -> Type) a = Transaction
  { unTransaction ::
      StateT
        TransactionCommits
        (ReaderT (TransactionContext mut imm) STM)
        a
  }
  deriving (Functor, Monad, Applicative)

newtype CasperT s root m a = CasperT (ReaderT (Store root) m a)
  deriving (Functor, Monad, Applicative, MonadIO)

loadStore :: FilePath -> (forall s. CasperT s root m a) -> m a
loadStore _ _ = undefined

liftSTM :: STM a -> Transaction mut imm a
liftSTM = Transaction . lift . lift

transact ::
  -- TODO hide ResourceRef and ContentRef in this constraint
  Content ResourceRef ContentRef (root ResourceRef ContentRef) =>
  ( forall mut imm.
    root mut imm ->
    Transaction mut imm a
  ) ->
  CasperT s root IO a
transact action = CasperT . ReaderT $ \(Store cache rootRef) -> do
  rLockSet' <- newTVarIO mempty
  cLockSet' <- newTVarIO mempty
  let ctx = TransactionContext rLockSet' cLockSet' cache id id id id
  (a, TransactionCommits commits) <- atomically . runTransaction ctx $ do
    root <- readMut rootRef
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
      TransactionContext mut imm ->
      Transaction mut imm a ->
      STM (a, TransactionCommits)
    runTransaction ctx (Transaction m) = runReaderT (runStateT m (TransactionCommits (pure ()))) ctx

-- transact' ::
--   -- TODO hide ResourceRef and ContentRef in this constraint
--   Content ResourceRef ContentRef (root ResourceRef ContentRef) =>
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

toResourceRef :: mut a -> Transaction mut imm (ResourceRef a)
toResourceRef mut = Transaction $ asks (($ mut) . txMutRef)

toUUID :: mut a -> Transaction mut imm UUID
toUUID mut = unDKey . unResourceRef <$> toResourceRef mut

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

getMutVar :: Content mut imm a => mut a -> IO (TVar a) -> Transaction mut imm (TVar a)
getMutVar mut createNewVar = Transaction $ do
  TransactionContext resourceLocks _ (Cache rcache _ rusers _) m2r r2m _ r2i <- ask
  let ResourceRef key = m2r mut
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

readMut :: forall mut imm a. Content mut imm a => mut a -> Transaction mut imm a
readMut mut = do
  TransactionContext _ _ _ _ hideMut _ hideImm <- Transaction ask
  uuid <- toUUID mut
  var <- getMutVar mut $ do
    bs <- readImmFromDisk uuid
    case decode (fmap hideMut . parseResourceRef) (fmap hideImm . parseContentRef) bs of
      Left err -> error err
      Right !a -> newTVarIO a
  liftSTM (readTVar var)

parseContentRef :: ByteString -> Either String (ContentRef r)
parseContentRef = error "not implemented"

parseResourceRef :: ByteString -> Either String (ResourceRef r)
parseResourceRef = error "not implemented"

writeMut :: Content mut imm a => mut a -> a -> Transaction mut imm ()
writeMut mut a = do
  var <- getMutVar mut $ newTVarIO a
  liftSTM $ writeTVar var a -- This is unfortunately a double write if we create a new TVar
  -- TODO add writeback action to commits

newMut :: a -> Transaction mut imm (mut a)
newMut = undefined

readImm :: imm a -> Transaction mut imm a
readImm = undefined

newImm :: a -> Transaction mut imm (imm a)
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
