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

-- | "Casper" is a content addresable store using ordinary directories, and content-addressible
-- files (files named after the SHA-1 hash of the file's conetnt) in the file system as the
-- database, similar to how the Git revision control system stores information. All filesystem
-- access through a single 'Store' handle in a single Haskell process is guaranteed to be thread
-- safe by the STM library. However __no guarantees__ are provided as to the consistency of the
-- database outside of the 'Store' handle of a single Haskell process, for example, if an
-- application were to create multiple 'Store's to the same filesystem directory accessible by
-- multiple threads, or if multiple processes were to access a single Casper store.
--
-- The 'openStore' function is the entry point to the Casper database, which evaluates functions of
-- type 'CasperT'. The 'Store' data type itself, and all types stored within (such as 'Ref' and
-- 'Var') have an @s@ type variable which serves the same purpose as the @s@ type variable of the
-- 'Control.Monad.ST.ST' function type: to prevent conflicting stateful updates to the Store
-- content. It is expected that a Casper store will be opened with 'openStore' in a separate process
-- thread only once, at process initialization, and a function in the 'CasperT' monad will loop
-- forever, listining for requests on an incoming channel for database lookups and updates.
--
-- To actually store and retrieve data with Casper, start by defining a @root@ data type that can
-- contain all of the information that is stored in the database, such as a list or a tree of
-- immutable 'Ref' values or mutable 'Var' values. Both 'Var' and 'Ref' data types can themselves be
-- stored into other data types that can be written to the store. By traversing through the @root@
-- data type, resolving 'Ref's and 'Var's as you go, you can extract pure data types from the store
-- You can also fold in the other direction, converting pure data types to storable data types.
-- 
-- The @root@ data type is stored into a single object in the database, and contains an index to all
-- other objects stored in the database. This @root@ object should be stored into an immutable
-- 'Var', unless the database is to be read-only. The "root" data type must take at least one type
-- variable @s@ which serves a similar purpose to the @s@ value of the 'Control.Monad.ST.ST'
-- monad.
--
-- It also is necessary for the "root" data type to derive several type classes, at a minimum:
--
--   - 'Content' from "Casper" in this "casper" package
--   - 'Generic' from "GHC.Generics" in "base"
--   - 'FromJSON' and 'ToJSON' from "Data.Aeson" in the "aeson" package
--   - 'Serialize' from "Data.Serialize" in the "cereal" package
--
-- The easiest way to instantiate these type classes is with the GHC language extensions:
--
--   - @DeriveGeneric@
--   - @DeriveAnyClass@
--   - @XDerivingStrategies@
--   - @DerivingVia@
--
-- Casper content is stored as 'ByteString's in the database. The "Casper" module provides a
-- 'WrapAeson' newtype that instantiates 'Serialize' in such a way that data is serialized as a JSON
-- string without prefixing the string with a binary integer string length. See the 'WrapAeson' data
-- type for more information.
--
-- Here is a template for a program that uses a Casper database:
--
-- @
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DerivingVia #-}
-- module Main where
-- import GHC.Generics (Generic)
-- import Data.Aeson (FromJSON, ToJSON) -- from the "aeson" package
-- import Data.Serialize (Serialize) -- from the "cereal" package
--
-- data Root s
--     = Root
--       { ...
--       }
--     deriving stock ('Generic')
--     deriving ('Content')
--     deriving anyclass ('FromJSON', 'ToJSON')
--     deriving ('Serialize') via WrapAeson (Root s)
--
-- emptyRoot :: Root s
-- emptyRoot = Root{ ... }
--
-- main :: IO ()
-- main = do
--     ...
--     (root, store) <- 'openStore' "/path/to/store" emptyRoot $ \ currentRoot -> do
--         -- store initial values
--         root <- transact $ newVar currentRoot
--         store <- 'getStore'
--         pure (root, store)
--     ...
--     forever $ do
--         ...
--         runCasperT store $ do
--             -- read/write operations on the Casper store
--         ...
-- @
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
    retain,
    liftSTM,
    collectGarbage,

    -- * Type classes and data types
    Content (..),
    WrapAeson (..),
    RawData(..),
    LazyRawData(..),
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
import Crypto.Hash (Digest, SHA256 (SHA256), hash, hashWith)
import DMap
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.ByteArray (withByteArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as Lazy
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as Serialize
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import qualified Data.Text.Encoding as Text
import Data.Typeable
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Foreign (peek)
import Foreign.Ptr (plusPtr)
import GHC.Conc (unsafeIOToSTM)
import GHC.Generics
import GHC.IO.Exception (IOErrorType (UserError))
import LargeWords (Word128 (..), Word256 (..))
import Ref
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile, openTempFile)
import Text.Read (readEither)
import Var

newtype TransactionID = TransactionID Word

-- | As the name implies, the a portion of state of the Casper 'Store' on disk is kept in memory to
-- reduce disk access.
data Cache = Cache
  { resourceCache :: TVar (DMap UUID),
    contentCache :: TVar (DMap SHA),
    resourceUsers :: UserTracker UUID, -- determines whether a resource can be freed from memory
    contentUsers :: UserTracker SHA, -- determines whether content can be freed from memory
    gcLock :: TVar Bool
  }

data Store s = Store {storeCache :: Cache, storePath :: FilePath}

-- | This is a thin wrapper that instnatiates the 'Serialize' class so that objects can be
-- serialized as a JSON string.
--
-- One problem with the default instantiation of 'Serialize' via 'ToJSON' prefixes the byte string
-- with the string length, written in the binary integer format. So JSON strings stored to files on
-- disk via 'Serialize' are always prefixed by a few bytes (the string length) that are rejected by
-- JSON parsers as garbage. This can cause problems with third-party utilities that may want to
-- inspect those JSON strings stored on disk.
--
-- This data type solves the problem. The 'Serialize' instance is defined such that the bytestring
-- length is not serialized, only the parsable characters are. This ensures that the JSON string
-- written by the 'Serialize' instance to a file on disk is compatible with any standard JSON
-- parser.
newtype WrapAeson a = WrapAeson {unWrapAeson :: a}

instance (FromJSON a, ToJSON a) => Serialize (WrapAeson a) where
  put = Serialize.putLazyByteString . Aeson.encode . unWrapAeson
  get = do
    i <- Serialize.remaining
    Serialize.getBytes i >>= either fail (pure . WrapAeson) . Aeson.eitherDecodeStrict'

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

-- | This monad is used any time the state of the Casper store needs to change, typically when
-- storing some 'Content' and obtaining it's 'Ref', as with 'newRef', when modifying the content of
-- a 'Var', as with 'writeVar'.
--
-- A 'Transaction' monad can be evaluated by the 'transact' function, which must itself be evaluated
-- in the 'CasperT' monad by the 'openStore' function. When a 'transact' function is live, the
-- 'Content' being written or read, individual 'Ref' and 'Var's are protected by software
-- transactional memory (STM) to ensure that the content being read will always be consistent, even
-- if other threads are writing to the store and perhaps modifying the same objects being read.
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

-- | Remove everything from the casper store that's not accessible from the set
-- of in-use resources.
collectGarbage :: MonadIO m => CasperT s m ()
collectGarbage = CasperT $
  ReaderT $ \(Store cache casperDir) -> do
    (objects, metas) <- liftIO $ do
      os <- listDirectory (casperDir </> "objects")
      ms <- listDirectory (casperDir </> "meta")
      pure (os, ms)
    let UserTracker inUse = resourceUsers cache
    inUse' <- liftIO $ readTVarIO inUse
    let roots = HashMap.keys inUse'
    liftIO $ atomically $ writeTVar (gcLock cache) True
    (uuids, shas) <-
      liftIO $
        foldM (\(us, ss) u -> sweepUUID casperDir us ss u) (HashSet.empty, HashSet.empty) roots
    liftIO $ atomically $ writeTVar (gcLock cache) False
    liftIO $ deleteInaccessible uuids shas (casperDir </> "objects") objects
    liftIO $ deleteInaccessible uuids shas (casperDir </> "meta") metas

sweepUUID :: FilePath -> HashSet UUID -> HashSet SHA -> UUID -> IO (HashSet UUID, HashSet SHA)
sweepUUID casperDir uuids shas uuid =
  if HashSet.member uuid uuids
    then pure (uuids, shas)
    else do
      ContentMeta luuids lshas <- readMetaFile (casperDir </> "meta" </> varFileName uuid)
      (uuids', shas') <- foldM (\(us, ss) u -> sweepUUID casperDir us ss u) (uuids, shas) luuids
      (uuids'', shas'') <- foldM (\(us, ss) s -> sweepSHA casperDir us ss s) (uuids', shas') lshas
      pure (uuids'', shas'')

sweepSHA :: FilePath -> HashSet UUID -> HashSet SHA -> SHA -> IO (HashSet UUID, HashSet SHA)
sweepSHA casperDir uuids shas sha =
  if HashSet.member sha shas
    then pure (uuids, shas)
    else do
      ContentMeta luuids lshas <- readMetaFile (casperDir </> "meta" </> show sha)
      (uuids', shas') <- foldM (\(us, ss) u -> sweepUUID casperDir us ss u) (uuids, shas) luuids
      (uuids'', shas'') <- foldM (\(us, ss) s -> sweepSHA casperDir us ss s) (uuids', shas') lshas
      pure (uuids'', shas'')

readMetaFile :: FilePath -> IO ContentMeta
readMetaFile fp = do
  bytes <- ByteString.readFile fp
  case Serialize.runGet Serialize.get bytes of
    Left err -> throwIO (userError err)
    Right c -> pure c

deleteInaccessible :: HashSet UUID -> HashSet SHA -> FilePath -> [FilePath] -> IO ()
deleteInaccessible uuids shas directory =
  traverse_
    ( \fp ->
        case readEither fp of
          Left _ -> case readEither fp of
            Left _ -> removeFile (directory </> fp)
            Right s -> unless (HashSet.member s shas) (removeFile (directory </> fp))
          Right u -> unless (HashSet.member u uuids) (removeFile (directory </> fp))
    )

-- | Retain a Var for the lifetime of the continuation by marking it as being
-- in use.
retain :: (MonadIO m, MonadMask m) => Transaction s (Var (a s) s) -> (forall x. Store x -> Var (a x) x -> m b) -> CasperT s m b
retain transaction runner = CasperT $
  ReaderT $ \store -> do
    let CasperT (ReaderT go) = transact (pin store transaction)
     in bracket (go store) (unpin store) (runner store)
  where
    -- We bump _within_ the transaction to prevent the variable to be garbage
    -- collected before the bump happens.
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

-- | This is a wrapper around a 'ByteString' that serizlizes content without a length prefix before
-- the bytes.
newtype RawData = RawData { unRawData :: ByteString }
  deriving (Eq, Ord)
  deriving newtype Show

instance Content RawData where refs _ _ _ = []

instance Serialize RawData where
  get = Serialize.remaining >>= fmap RawData . Serialize.getBytes
  put = Serialize.putByteString . unRawData

instance IsString RawData where fromString = RawData . fromString

-- | This is the same as 'RawData' but has a 'Lazy.ByteString' internally instead.
newtype LazyRawData = LazyRawData { unLazyRawData :: Lazy.ByteString }
  deriving (Eq, Ord)
  deriving newtype Show

instance Content LazyRawData where refs _ _ _ = []

instance IsString LazyRawData where fromString = LazyRawData . fromString

instance Serialize LazyRawData where
  get = Serialize.remaining >>= fmap LazyRawData . Serialize.getLazyByteString . fromIntegral
  put = Serialize.putLazyByteString . unLazyRawData


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

-- | Flush all cached content to disk, then empty out the cache.
emptyCache :: MonadIO m => m Cache
emptyCache = liftIO $ do
  resources <- newTVarIO mempty
  content <- newTVarIO mempty
  resourceUsers' <- UserTracker <$> newTVarIO mempty
  contentUsers' <- UserTracker <$> newTVarIO mempty
  lock' <- newTVarIO False
  pure $ Cache resources content resourceUsers' contentUsers' lock'

-- | This function initializes a Casper 'Store', provide a 'FilePath' for where you would like to
-- store the database in the filesystem. The @root@ value should be the data type that represents
-- all data in your database, this data type may contain an unlimited number of 'Var' or 'Ref'
-- values. Provide an empty @root@ value to this function in the event that the database does not
-- exist and it needs to be initialzed. If the database does already exist, the @root@ object given
-- to the continuation is constructed by deserializing the database root object from disk.
--
-- The continuation you provide to this function can perform additional initialization steps on the
-- 'Store', but be sure to evaluate 'getStore' to return the 'Store' handle from within the
-- 'CasperT' function context constructed by 'openStore'. context. The 'Store' handle that is
-- created allows you to perform further transactions using the 'runCasperT' function.
openStore ::
  ( forall s. Content (root s),
    forall s. Serialize (root s),
    MonadMask m,
    MonadIO m
  ) =>
  FilePath ->
  root x ->
  (forall s. Var (root s) s -> CasperT s m a) ->
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
          let rootUUID = UUID $ fromUUID rootId
          let rootVar = Var $ unsafeMkDKey rootUUID
          -- retain the root resource
          liftIO $ atomically $ bump rootUUID (resourceUsers cache)
          runCasperT (Store cache casperDir) (runner rootVar)
    else do
      rootId <- liftIO nextRandom
      let rootUUID = UUID $ fromUUID rootId
      let rootVar = Var $ unsafeMkDKey rootUUID
      runCasperT (Store cache casperDir) (transact (writeVar rootVar initial))
      liftIO $ Aeson.encodeFile manifest (CasperManifest "1" rootId)
      -- retain the root resource
      liftIO $ atomically $ bump rootUUID (resourceUsers cache)
      -- we don't need to debump the root after runner is finished because we
      -- drop the cache completely
      runCasperT (Store cache casperDir) (runner rootVar)

-- | Return the 'Store' handle for the current 'CasperT' context. This is most useful when obtaining
-- a 'Store' handle initialized by the 'openStore' function. The value returned by this function can
-- be used with the 'runCasperT' function to perform further database transactions.
getStore :: Applicative m => CasperT s m (Store s)
getStore = CasperT $ ReaderT pure

-- | Perform a read on the casper 'Store', or use with 'transact' to perform transactions that
-- update the state of the database.
runCasperT :: Store s -> CasperT s m a -> m a
runCasperT store (CasperT (ReaderT k)) = k store

liftSTM :: STM a -> Transaction s a
liftSTM = Transaction . lift . lift

-- | Evaluate a 'Transaction' function type, which can perform stateful updates on the Casper
-- 'Store' database.
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
      -- wait for GC to finish before we update any file
      let waitForGC = atomically $ do
            locked <- readTVar (gcLock cache)
            when locked retry
      traverse_
        ( \(k, (x, y)) ->
            let (fx, fy) = (tempVars HashMap.! k)
             in waitForGC *> moveTemp x fx *> moveTemp y fy
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
  TransactionContext resourceLocks _ (Cache rcache _ rusers _ _) _ <- ask
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

readSha :: Digest SHA256 -> IO SHA
readSha digest = withByteArray digest $ \ptr -> do
  wa <- peek ptr
  wb <- peek (ptr `plusPtr` 8)
  wc <- peek (ptr `plusPtr` 16)
  wd <- peek (ptr `plusPtr` 24)
  pure $ SHA (Word256 wa wb wc wd)

-- | Store a piece of 'Content' (which must be 'Serialize'-able) into the immutable portion of the
-- store. A reference to the object in the store is created and returned. This reference is a hash
-- of the content, so storing the exact same content will yield the exact same 'Ref'.
newRef :: (Content a, Serialize a) => a -> Transaction s (Ref a s)
newRef a = do
  TransactionContext _ _ _ casperDir <- Transaction ask
  let encoded = Serialize.encode a
  let digest = hashWith SHA256 encoded
  (sha, exists) <- liftSTM $
    safeIOToSTM $ do
      s <- readSha digest
      e <- doesFileExist (casperDir </> "objects" </> show s)
      pure (s, e)
  let commits = commitValue casperDir (show sha) encoded (meta a)
  unless exists $
    Transaction $
      modify $ \(TransactionCommits v r) ->
        TransactionCommits v (HashMap.insert sha commits r)
  pure $ Ref (unsafeMkDKey sha)

readRef :: (Serialize a, Content a) => Ref a s -> Transaction s a
readRef ref = do
  TransactionContext _ _ _ casperDir <- Transaction ask
  let Ref key = ref
  let sha = unDKey key
  liftSTM . safeIOToSTM $ do
    bs <- ByteString.readFile (casperDir </> "objects" </> show sha)
    case Serialize.decode bs of
      Left err ->
        error $
          unwords
            [ "corrupt content for",
              show sha <> ":",
              err
            ]
      Right !a -> pure a

readVar :: Serialize a => Var a s -> Transaction s a
readVar ref = do
  TransactionContext _ _ _ casperDir <- Transaction ask
  var <- getVar ref $ do
    let UUID uuid = unDKey $ unVar ref
    bs <- readVarFromDisk casperDir (UUID uuid)
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

readVarFromDisk :: FilePath -> UUID -> IO ByteString
readVarFromDisk casperDir uuid =
  let objectFile = casperDir </> "objects" </> varFileName uuid
   in ByteString.readFile objectFile

commitValue :: FilePath -> FilePath -> ByteString -> ContentMeta -> (WritePair, WritePair)
commitValue casperDir fileName a meta' =
  let commitContent =
        WritePair
          ( do
              createDirectoryIfMissing True (casperDir </> "tmp")
              (fp, h) <- openBinaryTempFile (casperDir </> "tmp") "varXXXXXX"
              ByteString.hPutStr h a
              hClose h
              pure fp
          )
          ( \fp -> do
              createDirectoryIfMissing True (casperDir </> "objects")
              renameFile fp (casperDir </> "objects" </> fileName)
          )
      commitMeta =
        WritePair
          ( do
              (fp, h) <- openBinaryTempFile (casperDir </> "tmp") "metaXXXXXX"
              ByteString.hPutStr h (Serialize.encode meta')
              hClose h
              pure fp
          )
          ( \fp -> do
              createDirectoryIfMissing True (casperDir </> "meta")
              renameFile fp (casperDir </> "meta" </> fileName)
          )
   in (commitContent, commitMeta)

varFileName :: UUID -> FilePath
varFileName (UUID uuid) = UUID.toString $ toUUID uuid

writeVar :: (Content a, Serialize a) => Var a s -> a -> Transaction s ()
writeVar var a = do
  tvar <- getVar var $ newTVarIO a
  liftSTM $ writeTVar tvar a -- This is unfortunately a double write if we create a new TVar
  TransactionContext _ _ _ casperDir <- Transaction ask
  let Var key = var
  let commits = commitValue casperDir (varFileName (unDKey key)) (Serialize.encode a) (meta a)
  Transaction $
    modify $ \(TransactionCommits v r) ->
      TransactionCommits (HashMap.insert (varUuid var) commits v) r

newVar' :: FilePath -> a -> IO (UUID, TVar a)
newVar' casperDir a = loop
  where
    loop = do
      u <- nextRandom
      let uuid = UUID $ fromUUID u
      -- ensure our UUID is not already in use
      exists <- doesFileExist (casperDir </> "objects" </> varFileName uuid)
      if exists
        then loop
        else do
          tvar <- newTVarIO a
          pure (uuid, tvar)

newVar :: (Content a, Serialize a) => a -> Transaction s (Var a s)
newVar a = do
  TransactionContext _ _ _ casperDir <- Transaction ask
  (uuid, tvar) <- liftSTM $ safeIOToSTM $ newVar' casperDir a
  let var = Var (unsafeMkDKey uuid)
  liftSTM $ writeTVar tvar a -- This is unfortunately a double write if we create a new TVar
  let Var key = var
  let commits = commitValue casperDir (varFileName (unDKey key)) (Serialize.encode a) (meta a)
  Transaction $
    modify $ \(TransactionCommits v r) ->
      TransactionCommits (HashMap.insert (varUuid var) commits v) r
  pure var

data ContentMeta = ContentMeta [UUID] [SHA]

instance Serialize ContentMeta where
  get = liftA2 ContentMeta Serialize.get Serialize.get
  put (ContentMeta uuids shas) = Serialize.put uuids *> Serialize.put shas

meta :: Content a => a -> ContentMeta
meta a =
  let (vars, refs') = unzip $ Content.refs (\(Var key) -> ([unDKey key], [])) (\(Ref key) -> ([], [unDKey key])) a
   in ContentMeta (concat vars) (concat refs')

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
