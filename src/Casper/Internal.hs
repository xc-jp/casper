{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Casper.Internal where

import Casper.Content
import Casper.Resource (Loc (Loc), Resource (encodeResource), decodeResource, resourceReferences)
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader (ReaderT (..), ask, asks)
import qualified Crypto.Hash as Crypto
import Data.Bits
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BShort
import qualified Data.ByteString.Unsafe as BU
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as U4
import Data.Word (Word64)
import GHC.Generics
import Numeric.Natural
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeFile,
  )
import System.FilePath ((</>))
import Text.Read (readMaybe)

data CasperError
  = FileCorrupt FilePath String
  | FileMissing FilePath
  | MetaCorrupt FilePath String
  | MetaMissing FilePath
  | StoreMetaCorrupt FilePath String
  | StoreMetaMissing FilePath
  deriving (Show, Eq)

newtype CasperT s root m a = CasperT {unCasperT :: ExceptT CasperError (ReaderT (Store root) m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CasperError, MonadCatch, MonadThrow, MonadMask)

instance MonadTrans (CasperT s root) where
  lift = CasperT . lift . lift

hashes :: Content a => a -> [SHA256]
hashes = references forget

newtype ContentMeta = ContentMeta {deps :: Set SHA256}

data ResourceMeta = ResourceMeta
  { blobSha :: SHA256,
    resourceDeps :: Set UUID.UUID,
    contentDeps :: Set SHA256
  }

encodeMeta :: ContentMeta -> BS.ByteString
encodeMeta (ContentMeta shas) =
  BL.toStrict $
    Builder.toLazyByteString $
      Builder.word64BE (fromIntegral $ S.size shas)
        <> foldMap (Builder.byteString . encodeSha) shas
  where
    encodeSha (SHA256 s) = BShort.fromShort s

decodeMeta :: BS.ByteString -> Either String ContentMeta
decodeMeta x = do
  (n, rest) <- getWord64BE x
  shas <- parseShas n rest
  pure $ ContentMeta $ S.fromList shas
  where
    parseShas 0 _ = pure []
    parseShas n bs = do
      (a, rest) <- ensure 32 bs
      let sha = SHA256 (BShort.toShort a)
      shas <- parseShas (pred n) rest
      pure (sha : shas)

encodeResourceMeta :: ResourceMeta -> BS.ByteString
encodeResourceMeta (ResourceMeta (SHA256 sha) ids shas) =
  BL.toStrict $
    Builder.toLazyByteString $
      Builder.byteString (BShort.fromShort sha)
        <> Builder.word64BE (fromIntegral $ S.size ids)
        <> foldMap (Builder.lazyByteString . encodeUUID) ids
        <> Builder.word64BE (fromIntegral $ S.size shas)
        <> foldMap (Builder.byteString . encodeSha) shas
  where
    encodeUUID = UUID.toByteString
    encodeSha (SHA256 s) = BShort.fromShort s

decodeResourceMeta :: BS.ByteString -> Either String ResourceMeta
decodeResourceMeta x = do
  (sha, rest) <- parseSha x
  (nids, rest') <- getWord64BE rest
  (uuids, rest'') <- parseUuids nids rest'
  (nshas, rest''') <- getWord64BE rest''
  (shas, _) <- parseShas nshas rest'''
  pure $ ResourceMeta sha (S.fromList uuids) (S.fromList shas)
  where
    parseSha bs = do
      (a, rest) <- ensure 32 bs
      let sha = SHA256 (BShort.toShort a)
      pure (sha, rest)
    parseShas 0 bs = pure ([], bs)
    parseShas n bs = do
      (sha, rest) <- parseSha bs
      (shas, rest') <- parseShas (pred n) rest
      pure (sha : shas, rest')
    parseUuid bs = do
      (a, rest) <- ensure 16 bs
      uuid <- maybe (Left "not a valid UUID") pure $ UUID.fromByteString (BL.fromStrict a)
      pure (uuid, rest)
    parseUuids 0 bs = pure ([], bs)
    parseUuids n bs = do
      (uuid, rest) <- parseUuid bs
      (uuids, rest') <- parseUuids (pred n) rest
      pure (uuid : uuids, rest')

ensure :: Int -> BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
ensure n bs =
  let (a, rest) = BS.splitAt n bs
   in if BS.length a == n
        then pure (a, rest)
        else Left $ "Failed to get " <> show n <> " bytes"

getWord64BE :: BS.ByteString -> Either String (Word64, BS.ByteString)
getWord64BE x = do
  (nBytes, rest) <- ensure 8 x
  let n = unsafeWord64be nBytes
  pure (n, rest)
  where
    unsafeWord64be :: BS.ByteString -> Word64
    unsafeWord64be s =
      (fromIntegral (s `BU.unsafeIndex` 0) `shiftL` 56)
        .|. (fromIntegral (s `BU.unsafeIndex` 1) `shiftL` 48)
        .|. (fromIntegral (s `BU.unsafeIndex` 2) `shiftL` 40)
        .|. (fromIntegral (s `BU.unsafeIndex` 3) `shiftL` 32)
        .|. (fromIntegral (s `BU.unsafeIndex` 4) `shiftL` 24)
        .|. (fromIntegral (s `BU.unsafeIndex` 5) `shiftL` 16)
        .|. (fromIntegral (s `BU.unsafeIndex` 6) `shiftL` 8)
        .|. fromIntegral (s `BU.unsafeIndex` 7)

deleteBlob :: MonadIO m => SHA256 -> CasperT s root m ()
deleteBlob sha = CasperT (asks $ blobPath sha) >>= lift . liftIO . removeFile

deleteObject :: MonadIO m => SHA256 -> CasperT s root m ()
deleteObject sha = CasperT (asks $ contentPath sha) >>= lift . liftIO . removeFile

deleteResource :: MonadIO m => UUID.UUID -> CasperT s root m ()
deleteResource uuid = CasperT (asks $ resourcePath uuid) >>= lift . liftIO . removeFile

collectGarbage :: (MonadMask m, MonadIO m) => Store s -> m (Either CasperError ())
collectGarbage s = do
  available' <- bracket_ takeGCLock releaseGCLock $ do
    finishActiveBlocks
    available s
  lockResources $
    runCasperT s $ do
      (resources, roots, blobs) <- resourceClosure
      contentClosure <- closure roots
      let notGarbage = S.union contentClosure blobs
      let (resources', objects', blobs') = available'
      forM_ (S.toList $ resources' S.\\ resources) deleteResource
      forM_ (S.toList $ blobs' S.\\ notGarbage) deleteBlob
      forM_ (S.toList $ objects' S.\\ contentClosure) deleteObject
  where
    lockResources = bracket_ takeResourceLock releaseResourceLock
    takeResourceLock = liftIO . atomically $ do
      locked <- readTVar (resourceLock s)
      when locked retry
      writeTVar (resourceLock s) True
    releaseResourceLock = liftIO . atomically $ do
      writeTVar (resourceLock s) False
    takeGCLock = liftIO . atomically $ do
      locked <- readTVar (gcLock s)
      when locked retry
      writeTVar (gcLock s) True
    releaseGCLock = liftIO $ atomically $ writeTVar (gcLock s) False
    finishActiveBlocks = liftIO . atomically $ do
      active <- readTVar (activeBlocks s)
      unless (active == 0) retry

available :: (MonadIO m) => Store root -> m (Set UUID.UUID, Set SHA256, Set SHA256)
available s = do
  resources <- liftIO $ listDirectory (storeDir s </> "resource")
  objects <- liftIO $ listDirectory (storeDir s </> "content")
  blobs <- liftIO $ listDirectory (storeDir s </> "blob")
  let uuids = mapMaybe readMaybe resources
  let metaShas = mapMaybe readMaybe objects
  let shas = mapMaybe readMaybe blobs
  pure (S.fromList uuids, S.fromList metaShas, S.fromList shas)

type ResourceState = (Set UUID.UUID, Set SHA256, Set SHA256)

-- | Collect all resources and resource blobs accessible from the root resource
-- as well as objects that are direct dependencies of any of the accessible
-- resources.
resourceClosure :: (MonadIO m) => CasperT s root m ResourceState
resourceClosure = go mempty [UUID.nil]
  where
    go checked [] = pure checked
    go (resources, objects, blobs) (h : t)
      | S.member h resources = go (resources, objects, blobs) t
      | otherwise = do
        ResourceMeta blob resources' objects' <- getResourceMeta h
        let transitive =
              ( S.insert h resources,
                S.union objects objects',
                S.insert blob blobs
              )
        go transitive (S.toList resources' <> t)

-- | Compute the transitive closure of a set of blob hashes
closure :: (MonadIO m, Foldable t) => t SHA256 -> CasperT s root m (Set SHA256)
closure = go mempty . toList
  where
    go checked [] = pure checked
    go checked (h : t)
      | S.member h checked = go checked t
      | otherwise = do
        transitive <- dependencies h
        go (S.insert h checked) (transitive <> t)
    dependencies sha = S.toList . deps <$> getMeta sha

mkMeta :: Content a => a -> ContentMeta
mkMeta a = ContentMeta (S.fromList $ hashes a)

hashBS :: BS.ByteString -> SHA256
hashBS = SHA256 . BShort.toShort . ByteArray.convert . Crypto.hashWith Crypto.SHA256

decodeFileWith ::
  (Monad m) =>
  -- | File reader
  (FilePath -> m (Either String a)) ->
  -- | Does path exist?
  (FilePath -> m Bool) ->
  -- | Path to file
  (Store root -> FilePath) ->
  -- | Missing path error
  (FilePath -> CasperError) ->
  -- | Corrupt file error
  (FilePath -> String -> CasperError) ->
  -- | Decoded value
  CasperT s root m a
decodeFileWith reader checkExist getPath eMissing eCorrupt = do
  path <- CasperT $ asks getPath
  exists <- lift $ checkExist path
  unless exists $ throwError (eMissing path)
  result <- lift $ reader path
  case result of
    Right a -> pure a
    Left err -> throwError (eCorrupt path err)

decodeFile ::
  (Content a, MonadIO m) =>
  -- | Path to file
  (Store root -> FilePath) ->
  -- | Missing path error
  (FilePath -> CasperError) ->
  -- | Corrupt path error
  (FilePath -> String -> CasperError) ->
  -- | Decoded value
  CasperT s root m a
decodeFile = decodeFileWith (fmap decodeContent . liftIO . BS.readFile) (liftIO . doesFileExist)

getMeta :: MonadIO m => SHA256 -> CasperT s root m ContentMeta
getMeta sha =
  decodeFileWith
    (fmap decodeMeta . liftIO . BS.readFile)
    (liftIO . doesFileExist)
    (contentPath sha)
    MetaMissing
    MetaCorrupt

getResourceMeta ::
  MonadIO m =>
  UUID.UUID ->
  CasperT s root m ResourceMeta
getResourceMeta uuid = CasperT $ do
  s <- ask
  let path = resourcePath uuid s
  exists <- liftIO . doesFileExist $ path
  unless exists $ throwError (MetaMissing path)
  result <- fmap decodeResourceMeta . liftIO . BS.readFile $ path
  either (throwError . MetaCorrupt path) pure result

-- | Recall the reference for a SHA256 hash in the store.
-- If the content is not present in the store 'Nothing' is returned.
recall :: MonadIO m => SHA256 -> CasperT s root m (Maybe (Ref s a))
recall sha = CasperT $ do
  content <- asks (blobPath sha) >>= liftIO . doesFileExist
  meta <- asks (contentPath sha) >>= liftIO . doesFileExist
  if content && meta then pure (Just (Ref sha)) else pure Nothing

recallRecursive :: MonadIO m => Ref' (f Ref') -> CasperT s root m (Maybe (Ref s (f (Ref s))))
recallRecursive (Ref' sha) = CasperT $ do
  content <- asks (blobPath sha) >>= liftIO . doesFileExist
  meta <- asks (contentPath sha) >>= liftIO . doesFileExist
  if content && meta then pure (Just (Ref sha)) else pure Nothing

-- | Retrieve content from the store using a reference
retrieve :: (Content a, MonadIO m) => Ref s a -> CasperT s root m a
retrieve (Ref sha) = decodeFile (blobPath sha) FileMissing FileCorrupt

blobPath :: SHA256 -> Store root -> FilePath
blobPath sha s = storeDir s </> "blob" </> show sha

contentPath :: SHA256 -> Store root -> FilePath
contentPath sha s = storeDir s </> "content" </> show sha

resourcePath :: UUID.UUID -> Store root -> FilePath
resourcePath uuid s = storeDir s </> "resource" </> show uuid

-- | Store content and obtain a reference to the stored value.
store ::
  (Content a, MonadMask m, MonadIO m) =>
  -- | Value to store
  a ->
  -- | Ref to stored value
  CasperT s root m (Ref s a)
store a = withFileLock sha . CasperT $ do
  exists <- asks (contentPath sha) >>= liftIO . doesFileExist
  unless exists $ do
    asks (blobPath sha) >>= writeCasper bs
    asks (contentPath sha) >>= writeCasper (encodeMeta meta)
  pure $ Ref sha
  where
    writeCasper content path = liftIO $ BS.writeFile path content
    bs = encodeContent a
    sha = hashBS bs
    meta = mkMeta a

withFileLock :: (MonadMask m, MonadIO m) => SHA256 -> CasperT s root m a -> CasperT s root m a
withFileLock sha run = do
  lockVar <- CasperT (asks contentLocks)
  bracket_ (acquire lockVar) (release lockVar) run
  where
    acquire lockVar = liftIO . atomically $ do
      locks <- readTVar lockVar
      let isLocked = S.member sha locks
      when isLocked retry
      modifyTVar lockVar (S.insert sha)
    release lockVar =
      liftIO . atomically $
        modifyTVar lockVar (S.delete sha)

getRoot :: Monad m => CasperT s root m (Loc s root)
getRoot = pure $ Loc UUID.nil

data Store root = Store
  { storeDir :: FilePath,
    -- | Lock that GC is currently traversing the directories as part of its
    -- marking phase, which means we cannot write new things to the store
    gcLock :: TVar Bool,
    -- | Locks on individual pieces of content, to prevent race conditions when
    -- writing the same content from two threads
    contentLocks :: TVar (Set SHA256),
    -- | Prevents modifications to resources while garbage collection is running
    resourceLock :: TVar Bool,
    -- | Locks on individual resources, to prevent race conditions when writing
    -- to the same resource from two threads
    resourceLocks :: TVar (Set UUID.UUID),
    -- | Number of open CasperT transactions, as per `runCasperT`
    activeBlocks :: TVar Natural
  }

resourcePath' :: UUID.UUID -> FilePath -> FilePath
resourcePath' uuid dir = dir </> "resource" </> show uuid

initStore :: Resource root => FilePath -> root -> IO (Store root)
initStore path r = do
  exists <- doesDirectoryExist path
  unless exists (createDirectory path)
  none <- emptyDirectory path
  when none $ do
    createDirectory (path </> "blob")
    createDirectory (path </> "content")
    createDirectory (path </> "resource")
    writeRootResource path r
  atomically $ do
    locked <- newTVar False
    active <- newTVar 0
    contentLocks' <- newTVar mempty
    resourceLock' <- newTVar False
    resourceLocks' <- newTVar mempty
    pure $ Store path locked contentLocks' resourceLock' resourceLocks' active
  where
    emptyDirectory = fmap null . listDirectory
    writeRootResource :: Resource root => FilePath -> root -> IO ()
    writeRootResource dir x = do
      BS.writeFile (dir </> "blob" </> show sha) bytes
      BS.writeFile (dir </> "resource" </> show UUID.nil) (encodeResourceMeta $ resourceMeta sha x)
      where
        bytes = encodeResource x
        sha = hashBS bytes

-- | Create a new resource initalized with content.
newResource :: (MonadIO m, MonadMask m, Resource a) => a -> CasperT s root m (Loc s a)
newResource x = go
  where
    go = do
      uuid <- liftIO U4.nextRandom
      s <- CasperT ask
      exists <- liftIO . doesFileExist $ resourcePath uuid s
      if exists
        then go -- try with another uuid
        else Loc uuid <$ writeResource (Loc uuid) x

-- | Write resource without locking
unsafeWriteResource :: (Resource a, MonadMask m, MonadIO m) => UUID.UUID -> a -> CasperT s root m ()
unsafeWriteResource uuid x = do
  s <- CasperT ask
  withFileLock sha . liftIO $ do
    exists <- liftIO . doesFileExist $ blobPath sha s
    unless exists $ BS.writeFile (blobPath sha s) bs
    BS.writeFile (resourcePath uuid s) (encodeResourceMeta meta)
  where
    bs = encodeResource x
    sha = hashBS bs
    meta = resourceMeta sha x

unsafeReadResource :: (Resource a, MonadMask m, MonadIO m) => UUID.UUID -> CasperT s root m a
unsafeReadResource uuid = do
  ResourceMeta sha _ _ <- getResourceMeta uuid
  decodeResourceFile (blobPath sha) FileMissing FileCorrupt
  where
    decodeResourceFile =
      decodeFileWith (fmap decodeResource . liftIO . BS.readFile) (liftIO . doesFileExist)

-- | Write a value to a resource
writeResource :: (Resource a, MonadMask m, MonadIO m) => Loc s a -> a -> CasperT s root m ()
writeResource (Loc uuid) x = waitOnBigLock $ withResourceLock uuid $ unsafeWriteResource uuid x
  where
    waitOnBigLock act = do
      bigLock <- CasperT (asks resourceLock)
      liftIO . atomically $ do
        locked <- readTVar bigLock
        when locked retry
      act

-- | Read the value of a resource at a location
readResource :: (Resource a, MonadMask m, MonadIO m) => Loc s a -> CasperT s root m a
readResource (Loc uuid) = withResourceLock uuid $ unsafeReadResource uuid

-- | Modify a resource
modifyResource :: (Resource a, MonadMask m, MonadIO m) => (a -> a) -> Loc s a -> CasperT s root m ()
modifyResource f (Loc uuid) = withResourceLock uuid $ do
  x <- unsafeReadResource uuid
  unsafeWriteResource uuid (f x)

-- | Spot the location of a resource in the store given its UUID.
-- If the resource is not present in the store 'Nothing' is returned.
spot :: (Resource a, MonadIO m) => UUID.UUID -> CasperT s root m (Maybe (Loc s a))
spot uuid = CasperT $ do
  meta <- asks (resourcePath uuid) >>= liftIO . doesFileExist
  if meta then pure (Just (Loc uuid)) else pure Nothing

withResourceLock :: (MonadMask m, MonadIO m) => UUID.UUID -> CasperT s root m a -> CasperT s root m a
withResourceLock uuid run = do
  lockVar <- CasperT (asks resourceLocks)
  bracket_ (acquire lockVar) (release lockVar) run
  where
    acquire lockVar = liftIO . atomically $ do
      locks <- readTVar lockVar
      let isLocked = S.member uuid locks
      when isLocked retry
      modifyTVar lockVar (S.insert uuid)
    release lockVar =
      liftIO . atomically $
        modifyTVar lockVar (S.delete uuid)

resourceMeta :: Resource a => SHA256 -> a -> ResourceMeta
resourceMeta sha a =
  let (locs, refs) = resourceReferences (\(Loc u) -> u) forget a
   in ResourceMeta sha (S.fromList locs) (S.fromList refs)

runCasperT :: (MonadMask m, MonadIO m) => Store root -> (forall s. CasperT s root m a) -> m (Either CasperError a)
runCasperT s (CasperT action) = do
  withActiveBlock s $
    runReaderT (runExceptT action) s

withActiveBlock :: (MonadMask m, MonadIO m) => Store root -> m a -> m a
withActiveBlock s = bracket_ acquire release
  where
    acquire =
      liftIO . atomically $ do
        locked <- readTVar $ gcLock s
        when locked retry
        modifyTVar (activeBlocks s) succ
    release =
      liftIO $ atomically $ modifyTVar (activeBlocks s) pred

newtype Ref' a = Ref' {getRef' :: SHA256}

newtype MaybeT m a = MaybeT {unMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT x) = MaybeT $ fmap (fmap f) x

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT $ pure (pure x)
  MaybeT fs <*> MaybeT as = MaybeT ((<*>) <$> fs <*> as)

class Recall f where
  recalling ::
    Applicative m =>
    (forall g. ref (g ref) -> m (ref' (g ref'))) ->
    (forall a. ref a -> m (ref' a)) ->
    (f ref -> m (f ref'))

recallCasper :: (Recall f, MonadIO m) => f Ref' -> CasperT s root m (Maybe (f (Ref s)))
recallCasper = unMaybeT . recalling (MaybeT . recallRecursive) (MaybeT . recall . getRef')

----- DELETE EVERYTHING BELOW THIS LINE -----

newtype Datapoint ref = Datapoint (ref BS.ByteString)
  deriving (Generic)

data Datapair ref = Datapair (ref (Datapoint ref)) (ref Bool)

newtype Dataclass ref = Dataclass [Datapoint ref]

instance Recall Dataclass where
  recalling g f (Dataclass points) = Dataclass <$> traverse (recalling g f) points

newtype Dataset ref = Dataset (ref (Dataclass ref))

instance Recall Dataset where
  recalling inner _ (Dataset d) = Dataset <$> inner d

instance Recall Datapoint where
  recalling _ g (Datapoint d) = Datapoint <$> g d

instance Recall Datapair where
  recalling f g (Datapair a b) = Datapair <$> f a <*> g b

newtype Root ref = Root (Datapair ref)

instance Recall Root where
  recalling f g (Root p) = Root <$> recalling f g p

{-
instance Content' (Datapair rec f) (Datapair rec g) f g where
  refs f (Datapair p b) = Datapair <$> f p <*> f b

instance Content' (Root rec f) (Root rec g) f g where
  refs f (Root p) = Root <$> f p
  -}

recallDatapair :: MonadIO m => Datapair Ref' -> MaybeT (CasperT s root m) (Datapair (Ref s))
recallDatapair = recalling (MaybeT . recallRecursive) (MaybeT . recall . getRef')

{-
recallDatapair :: Datapair Ref' Ref' -> M s (Datapair Ref' (Ref s))
recallDatapair = refs recall

recallRoot :: Root Ref' Ref' -> M s (Root Ref' (Ref s))
recallRoot = refs recall

fetchDatapair :: Root Ref' (Ref s) -> M s (Datapair Ref' (Ref s))
fetchDatapair (Root dp) = retrieve dp >>= refs recall
-}

{-
instance Content' (Datapoint f) (Datapoint g) (f BS.ByteString) (g BS.ByteString) where
  refs f (Datapoint ref) = Datapoint <$> f ref
  -}
