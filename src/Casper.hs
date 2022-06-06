{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Casper
  ( -- * transactions
    Transaction,
    transact,

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
    openStore,
    getStore,
    runCasperT,
    liftSTM,

    -- * Type classes and data types
    Content (..),
    WrapAeson (..),
  )
where

import Content
import Control.Applicative (liftA2)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (throwIO)
import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader
import Control.Monad.State
import DMap
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text.Encoding as Text
import Data.Typeable
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import GHC.Conc (unsafeIOToSTM)
import GHC.Generics
import GHC.IO.Exception (IOErrorType (UserError))
import LargeWords (Word128 (..), Word256 (..))
import Ref
import System.Directory (doesFileExist, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile, openTempFile)
import Var

newtype TransactionID = TransactionID Word

data Cache = Cache
  { resourceCache :: TVar (DMap UUID),
    contentCache :: TVar (DMap SHA),
    resourceUsers :: UserTracker UUID, -- determines whether a resource can be freed from memory
    contentUsers :: UserTracker SHA -- determines whether content can be freed from memory
  }

data Store s = Store {storeCache :: Cache, storePath :: FilePath}

newtype WrapAeson a = WrapAeson {unWrapAeson :: a}

instance (FromJSON a, ToJSON a) => Serialize (WrapAeson a) where
  put = Serialize.put . Aeson.encode . unWrapAeson
  get = Serialize.get >>= either fail (pure . WrapAeson) . Aeson.eitherDecodeStrict'

data TransactionCommits = TransactionCommits
  { varCommits :: HashMap UUID (WritePair, WritePair),
    refCommits :: HashMap SHA (WritePair, WritePair)
  }

data WritePair = WritePair {writeTemp :: IO FilePath, moveTemp :: FilePath -> IO ()}

writeBracket :: WritePair -> (FilePath -> IO ()) -> IO ()
writeBracket (WritePair write move) = Exception.bracket write move

data TransactionContext = TransactionContext
  { txResourceLocks :: TVar (HashSet UUID), -- FIXME: rename, this is about resources used by this transaction
    txContentLocks :: TVar (HashSet SHA),
    txCache :: Cache,
    txStorePath :: FilePath
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

deriving instance MonadMask m => MonadMask (CasperT s m)

deriving instance MonadCatch m => MonadCatch (CasperT s m)

deriving instance MonadThrow m => MonadThrow (CasperT s m)

-- | Retain a Var for the lifetime of the continuation by marking it as being
-- in use.
retain :: (MonadIO m, MonadMask m) => Transaction s (Var a s) -> (forall x. Store x -> Var a x -> m b) -> CasperT s m b
retain transaction runner = CasperT $
  ReaderT $ \store -> do
    let CasperT (ReaderT go) = transact (pin store transaction)
     in bracket (go store) (unpin store) (runner store)
  where
    -- We bump _within_ the transaction to prevent the variable to be garbage
    -- collected before the bump happens.
    pin :: Store s -> Transaction s (Var a s) -> Transaction s (Var a s)
    pin (Store c _) t = do
      var@(Var key) <- t
      let uuid = unDKey key
      liftSTM $ bump uuid (resourceUsers c)
      pure var

    unpin (Store c _) (Var key) =
      let uuid = unDKey key
       in void . liftIO . atomically $ debump uuid (resourceUsers c)

-- data
--  |-casper.json
--  |-objects
--  | |- <root>
--  | |- <sha0>
--  |-meta
--    |- <root>
--    |- <sha0>
--

data CasperManifest = CasperManifest
  { casperVersion :: String,
    rootResource :: UUID.UUID
  }

instance ToJSON CasperManifest where
  toJSON (CasperManifest v r) =
    Aeson.object
      [ Key.fromString "version" Aeson..= v,
        Key.fromString "root" Aeson..= r
      ]

instance FromJSON CasperManifest where
  parseJSON value = do
    o <- Aeson.parseJSON value
    v <- o Aeson..: "version"
    r <- o Aeson..: "root"
    checkVersion v
    pure $ CasperManifest v r

checkVersion :: MonadFail m => String -> m ()
checkVersion "1" = pure ()
checkVersion v = fail $ "Unknown casper version: " <> v

emptyCache :: MonadIO m => m Cache
emptyCache = liftIO $ do
  resources <- newTVarIO mempty
  content <- newTVarIO mempty
  resourceUsers' <- UserTracker <$> newTVarIO mempty
  contentUsers' <- UserTracker <$> newTVarIO mempty
  pure $ Cache resources content resourceUsers' contentUsers'

openStore ::
  ( forall s. Content (root s),
    forall s. Serialize (root s),
    MonadMask m,
    MonadIO m
  ) =>
  FilePath ->
  root x ->
  (forall s. root s -> CasperT s m a) ->
  m a
openStore casperDir initial runner = do
  let manifest = casperDir </> "casper.json"
  exists <- liftIO $ doesFileExist manifest
  cache <- emptyCache
  if exists
    then do
      result <- liftIO $ Aeson.eitherDecodeFileStrict' manifest
      case result of
        Left err -> liftIO $ throwIO (userError err)
        Right (CasperManifest _ rootId) -> do
          let rootVar = Var $ unsafeMkDKey (UUID $ fromUUID rootId)
          runCasperT (Store cache casperDir) (transact (readVar rootVar) >>= runner)
    else do
      rootId <- liftIO nextRandom
      let rootVar = Var $ unsafeMkDKey (UUID $ fromUUID rootId)
      runCasperT (Store cache casperDir) (transact (writeVar rootVar initial))
      liftIO $ Aeson.encodeFile manifest (CasperManifest "1" rootId)
      runCasperT (Store cache casperDir) (runner initial)

getStore :: Applicative m => CasperT s m (Store s)
getStore = CasperT $ ReaderT pure

runCasperT :: Store s -> CasperT s m a -> m a
runCasperT store (CasperT (ReaderT k)) = k store

liftSTM :: STM a -> Transaction s a
liftSTM = Transaction . lift . lift

transact ::
  (MonadIO m, MonadMask m) =>
  Transaction s a ->
  CasperT s m a
transact transaction = CasperT . ReaderT $ \(Store cache storePath') -> do
  bracket enter (exit cache) $ \(rLockSet', cLockSet') -> do
    (a, TransactionCommits ref var) <-
      let ctx = TransactionContext rLockSet' cLockSet' cache storePath'
       in liftIO . atomically . runTransaction ctx $ transaction
    liftIO $ do
      tempRefs <- traverse (\(x, y) -> liftA2 (,) (writeTemp x) (writeTemp y)) ref
      tempVars <- traverse (\(x, y) -> liftA2 (,) (writeTemp x) (writeTemp y)) var
      traverse_
        ( \(k, (x, y)) ->
            let (fx, fy) = (tempRefs HashMap.! k)
             in moveTemp x fx *> moveTemp y fy
        )
        (HashMap.toList ref)
      traverse_
        ( \(k, (x, y)) ->
            let (fx, fy) = (tempVars HashMap.! k)
             in moveTemp x fx *> moveTemp y fy
        )
        (HashMap.toList var)
    pure a
  where
    enter = liftIO $ do
      liftA2 (,) (newTVarIO mempty) (newTVarIO mempty)
    exit cache (rLockSet', cLockSet') = liftIO $ do
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
getVar var createNewTVar = Transaction $ do
  TransactionContext resourceLocks _ (Cache rcache _ rusers _) _ <- ask
  let Var key = var
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
        tvar <- createNewTVar
        -- Just in case the resource was inserted while we were reading/decoding it,
        -- we recheck the cache before inserting
        atomically $ do
          mCachedVar' <- DMap.lookup key <$> readTVar rcache
          case mCachedVar' of
            Nothing -> do
              modifyTVar rcache (DMap.insert tvar key)
              pure tvar
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
  TransactionContext _ _ _ casperDir <- Transaction ask
  let varFile = casperDir </> "objects" </> varFileName var
  let commitContent =
        WritePair
          ( do
              (fp, h) <- openBinaryTempFile (casperDir </> "tmp") "varXXXXXX"
              ByteString.hPutStr h (Serialize.encode a)
              hClose h
              pure fp
          )
          (`renameFile` varFile)
  let commitMeta =
        WritePair
          ( do
              (fp, h) <- openBinaryTempFile (casperDir </> "tmp") "metaXXXXXX"
              ByteString.hPutStr h (Serialize.encode (meta a))
              hClose h
              pure fp
          )
          (`renameFile` varFile)
  Transaction $
    modify $ \(TransactionCommits v r) ->
      TransactionCommits (HashMap.insert (varUuid var) (commitContent, commitMeta) v) r

data ContentMeta = ContentMeta [UUID] [SHA]

-- TODO
instance Serialize ContentMeta where
  get = undefined
  put = undefined

meta :: Content a => a -> ContentMeta
meta a =
  let (vars, refs') = unzip $ Content.refs (\(Var key) -> ([unDKey key], [])) (\(Ref key) -> ([], [unDKey key])) a
   in ContentMeta (concat vars) (concat refs')

varFileName :: Var a s -> FilePath
varFileName (Var key) =
  let UUID uuid = unDKey key
   in UUID.toString $ toUUID uuid

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
