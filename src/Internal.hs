{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal where

import Content
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader (ReaderT (..), asks)
import qualified Crypto.Hash as Crypto
import Data.Bits
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BShort
import qualified Data.ByteString.Unsafe as BU
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word64)
import Numeric.Natural
import System.Directory
import System.FilePath

data CasperError
  = FileCorrupt FilePath String
  | FileMissing FilePath
  | MetaCorrupt FilePath String
  | MetaMissing FilePath
  | StoreMetaCorrupt FilePath String
  | StoreMetaMissing FilePath
  deriving (Show, Eq)

newtype CasperT m a = CasperT {unCasperT :: ExceptT CasperError (ReaderT Store m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CasperError, MonadCatch, MonadThrow, MonadMask)

instance MonadTrans CasperT where
  lift = CasperT . lift . lift

references' :: Content a => a -> [SHA256]
references' = references forget

newtype ContentMeta = ContentMeta {deps :: Set SHA256}

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

closure :: MonadIO m => [SHA256] -> CasperT m (Set SHA256)
closure = go mempty
  where
    go checked [] = pure checked
    go checked (h : t)
      | S.member h checked = go checked t
      | otherwise = do
        transitive <- dependencies h
        go (S.insert h checked) (transitive <> t)
    dependencies sha = S.toList . deps <$> getMeta sha

mkMeta :: Content a => a -> ContentMeta
mkMeta a = ContentMeta (S.fromList $ references' a)

hashBS :: BS.ByteString -> SHA256
hashBS = SHA256 . BShort.toShort . ByteArray.convert . Crypto.hashWith Crypto.SHA256

delete :: MonadIO m => SHA256 -> CasperT m ()
delete sha = do
  CasperT (asks $ blobPath sha) >>= lift . liftIO . removeFile
  CasperT (asks $ metaPath sha) >>= lift . liftIO . removeFile

collectGarbage :: (MonadMask m, MonadIO m) => Store -> m (Set SHA256) -> m (Either CasperError ())
collectGarbage s getRoots = do
  liftIO . atomically $ do
    locked <- readTVar (gcLock s)
    when locked retry
    writeTVar (gcLock s) True
  liftIO . atomically $ do
    active <- readTVar (activeBlocks s)
    unless (active == 0) retry
  rs <- getRoots
  everything <- getEverything
  liftIO $ atomically $ writeTVar (gcLock s) False
  runCasperT s $ do
    notGarbage <- closure (S.toList rs)
    forM_ (S.toList $ everything S.\\ notGarbage) delete
  where
    getEverything :: MonadIO m => m (Set SHA256)
    getEverything = do
      paths <- liftIO $ listDirectory (storeRoot s </> "blob")
      let shas = fmap read paths
      pure $ S.fromList shas

decodeFileWith ::
  (Monad m) =>
  -- | File reader
  (FilePath -> m (Either String a)) ->
  -- | Does path exist?
  (FilePath -> m Bool) ->
  -- | Path to file
  (Store -> FilePath) ->
  -- | Missing path error
  (FilePath -> CasperError) ->
  -- | Corrupt file error
  (FilePath -> String -> CasperError) ->
  -- | Decoded value
  CasperT m a
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
  (Store -> FilePath) ->
  -- | Missing path error
  (FilePath -> CasperError) ->
  -- | Corrupt path error
  (FilePath -> String -> CasperError) ->
  -- | Decoded value
  CasperT m a
decodeFile = decodeFileWith (fmap decodeContent . liftIO . BS.readFile) (liftIO . doesFileExist)

getMeta :: MonadIO m => SHA256 -> CasperT m ContentMeta
getMeta sha = do
  decodeFileWith (fmap decodeMeta . liftIO . BS.readFile) (liftIO . doesFileExist) (metaPath sha) MetaMissing MetaCorrupt

-- | Recall the reference for a SHA256 hash in the store.
-- If the content is not present in the store 'Nothing' is returned.
recall :: (Content a, MonadIO m) => SHA256 -> CasperT m (Maybe (Ref a))
recall sha = CasperT $ do
  content <- asks (blobPath sha) >>= liftIO . doesFileExist
  meta <- asks (metaPath sha) >>= liftIO . doesFileExist
  if content && meta then pure (Just (Ref sha)) else pure Nothing

retrieve :: (Content a, MonadIO m) => Ref a -> CasperT m a
retrieve (Ref sha) = decodeFile (blobPath sha) FileMissing FileCorrupt

blobPath :: SHA256 -> Store -> FilePath
blobPath sha s = storeRoot s </> "blob" </> show sha

metaPath :: SHA256 -> Store -> FilePath
metaPath sha s = storeRoot s </> "meta" </> show sha

store ::
  (Content a, MonadMask m, MonadIO m) =>
  -- | Value to store
  a ->
  -- | Ref to stored value
  CasperT m (Ref a)
store a = withFileLock sha . CasperT $ do
  exists <- asks (metaPath sha) >>= liftIO . doesFileExist
  unless exists $ do
    asks (blobPath sha) >>= writeCasper bs
    asks (metaPath sha) >>= writeCasper (encodeMeta meta)
  pure $ Ref sha
  where
    writeCasper content path = liftIO $ BS.writeFile path content
    bs = encodeContent a
    sha = hashBS bs
    meta = mkMeta a

withFileLock :: (MonadMask m, MonadIO m) => SHA256 -> CasperT m a -> CasperT m a
withFileLock sha run = do
  lockVar <- CasperT (asks fileLocks)
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

data Store = Store
  { storeRoot :: FilePath,
    gcLock :: TVar Bool,
    fileLocks :: TVar (Set SHA256),
    activeBlocks :: TVar Natural
  }

initStore :: FilePath -> IO Store
initStore path = do
  exists <- doesDirectoryExist path
  unless exists (createDirectory path)
  none <- emptyDirectory path
  when none $ do
    createDirectory (path </> "blob")
    createDirectory (path </> "meta")
  atomically $ do
    locked <- newTVar False
    active <- newTVar 0
    locks <- newTVar mempty
    pure $ Store path locked locks active
  where
    emptyDirectory = fmap null . listDirectory

runCasperT :: (MonadMask m, MonadIO m) => Store -> CasperT m a -> m (Either CasperError a)
runCasperT s (CasperT action) = do
  withActiveBlock s $
    runReaderT (runExceptT action) s

withActiveBlock :: (MonadMask m, MonadIO m) => Store -> m a -> m a
withActiveBlock s = bracket_ acquire release
  where
    acquire =
      liftIO . atomically $ do
        locked <- readTVar $ gcLock s
        when locked retry
        modifyTVar (activeBlocks s) succ
    release =
      liftIO $ atomically $ modifyTVar (activeBlocks s) pred
