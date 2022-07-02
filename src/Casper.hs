{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Casper where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import Data.DMap (DMap, unDKey)
import qualified Data.DMap as DMap
import Data.Ref
import Data.Var
import Data.Void
import GHC.Conc
import qualified GHC.Weak as Weak
import Unsafe.Coerce (unsafeCoerce)
import Util

newtype Transaction t s a = Transaction
  { unTransaction ::
      StateT
        TransactionState
        (ReaderT (TransactionContext s) STM)
        a
  }
  deriving newtype (Functor, Applicative, Monad, MonadSTM)

data TransactionContext s = TransactionContext
  { txStore :: Store s,
    txRefRoots :: TVar (DMap SHA),
    txVarRoots :: TVar (DMap UUID)
  }

data TransactionState

data Store s = Store
  { rootPath :: FilePath,
    -- TODO
    -- The entire cache is hidden behind a single TVar.
    -- That means that every time the cache structurally changes, any other actions are retried.
    -- This is annoying because the cache is involed in every transactiont.
    -- Doubly so because conceptually, most transactions only affect a tiny part of the cache, and therefore don't actually interfere with one another.
    -- Since the cache is a hash map, it should be possible to have something more STM-aware/friendly here.
    -- Let's first get some benchmarks though.
    varCache :: TVar (DMap UUID),
    refCache :: TVar (DMap SHA)
  }

data LiveVar a s = LiveVar
  { lvTVar :: TVar (a Void),
    lvVar :: Var a Void
  }

data LiveRef a s = LiveRef
  { lrSHA :: SHA,
    lvRef :: TVar (a Void)
  }

-- TODO Does it make sense to first check the transaction-local cache?
newVar :: a t -> Transaction t s (LiveVar a s)
newVar a = Transaction $ do
  vcache' <- asks (varCache . txStore)
  let a' = unsafeRescope a
  liftSTM $ do
    var <- unsafeIOToSTM nextVar
    tvar <- newTVar a'
    weak <- unsafeIOToSTM (mkWeakTVar tvar (finalizer var tvar vcache'))
    modifyTVar' vcache' (DMap.insert weak (unVar var))
    pure $ LiveVar tvar var

toVar :: LiveVar a s -> Var a t
toVar = rescopeVar . lvVar

-- TODO Does it make sense to first check the transaction-local cache?
loadVar :: Var a t -> Transaction t s (LiveVar a s)
loadVar var@(Var key) = Transaction $ do
  vcache' <- asks (varCache . txStore)
  mref <- liftSTM . unsafeIOToSTM $ do
    vcache <- readTVarIO vcache'
    case DMap.lookup key vcache of
      Nothing -> pure Nothing
      Just weak -> Weak.deRefWeak weak
  livevar <- case mref of
    Just ref -> pure $ LiveVar ref (rescopeVar var)
    Nothing -> liftSTM . safeIOToSTM $ do
      a <- loadFromDisk (unDKey key)
      tvarNew <- newTVarIO a
      weakNew <- mkWeakTVar tvarNew (finalizer var tvarNew vcache')
      atomically $ do
        vcache <- readTVar vcache'
        let useNewVar = LiveVar tvarNew (rescopeVar var) <$ writeTVar vcache' (DMap.insert weakNew key vcache)
        case DMap.lookup key vcache of
          Nothing -> useNewVar
          Just weakOld ->
            unsafeIOToSTM (Weak.deRefWeak weakOld) >>= \case
              Nothing -> useNewVar
              Just tvarOld -> pure (LiveVar tvarOld (rescopeVar var))
  localCache <- asks txVarRoots
  pure livevar
  where
    loadFromDisk :: UUID -> IO (a s)
    loadFromDisk = undefined

finalizer :: Var a s -> TVar (a Void) -> TVar (DMap UUID) -> IO ()
finalizer (Var key) tvar vcache' = do
  atomically $ do
    vcache <- readTVar vcache'
    case DMap.lookup key vcache of
      Nothing -> pure ()
      Just weak -> do
        tvar' <- unsafeIOToSTM (Weak.deRefWeak weak)
        when (Just tvar == tvar') $ writeTVar vcache' (DMap.delete key vcache)

safeIOToSTM :: IO a -> STM a
safeIOToSTM = undefined

readVar :: LiveVar a s -> Transaction t s (a t)
readVar lv = do
  -- read
  -- mark as local root
  undefined

readVarIO :: LiveVar a s -> (forall t. a t -> r) -> IO r
readVarIO lv f = f <$> readTVarIO (lvTVar lv)

writeVar :: LiveVar a s -> a t -> Transaction t s ()
writeVar = undefined

transact :: Store s -> (forall t. Transaction t s a -> IO a)
transact _ = undefined

unsafeRescope :: a s -> a t
unsafeRescope = unsafeCoerce
