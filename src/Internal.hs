{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal where

import Content
import Control.Monad.Except
import Control.Monad.Reader
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import Data.Serialize
import Data.Set (Set)
import qualified Data.Set as S
import System.Directory
import System.FilePath
import System.IO.Error (alreadyExistsErrorType, mkIOError)

data CasperError
  = FileCorrupt FilePath String
  | FileMissing FilePath
  | MetaCorrupt FilePath String
  | MetaMissing FilePath
  | StoreMetaCorrupt FilePath String
  | StoreMetaMissing FilePath
  deriving (Show, Eq)

roundTrip :: [Int] -> CasperT IO [Int]
roundTrip is = do
  refs <- traverse store is
  ref <- store refs
  retrieve ref >>= traverse retrieve

newtype CasperT m a = CasperT {unCasperT :: ExceptT CasperError (ReaderT Store m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CasperError)

instance MonadTrans CasperT where
  lift = CasperT . lift . lift

references' :: Content a => a -> [SHA256]
references' = references forget

newtype ContentMeta = ContentMeta {deps :: Set SHA256}
  deriving (Serialize)

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
hashBS = SHA256 . Base16.encode . SHA256.hash

delete :: MonadIO m => SHA256 -> CasperT m ()
delete sha = do
  CasperT (asks $ blobPath sha) >>= lift . liftIO . removeFile
  CasperT (asks $ metaPath sha) >>= lift . liftIO . removeFile

-- future optimization: traverse shared deps only once
collectGarbage :: MonadIO m => CasperT m ()
collectGarbage = do
  rs <- roots
  everything <- getEverything
  notGarbage <- closure (S.toList rs)
  forM_ (S.toList $ everything S.\\ notGarbage) delete
  where
    getEverything :: MonadIO m => CasperT m (Set SHA256)
    getEverything = CasperT $ do
      root <- asks storeRoot
      paths <- liftIO $ listDirectory root
      let paths = filter (not . hasExtension) paths
      let shas = fmap (SHA256 . Char8.pack) paths
      pure $ S.fromList shas

newtype StoreMeta = StoreMeta {storeRoots :: Set SHA256}
  deriving (Serialize)

decodeFileWith ::
  (Serialize a, Monad m) =>
  -- | File reader
  (FilePath -> m BS.ByteString) ->
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
  bs <- lift $ reader path
  case decode bs of
    Right a -> pure a
    Left err -> throwError (eCorrupt path err)

decodeFile ::
  (Serialize a, MonadIO m) =>
  -- | Path to file
  (Store -> FilePath) ->
  -- | Missing path error
  (FilePath -> CasperError) ->
  -- | Corrupt path error
  (FilePath -> String -> CasperError) ->
  -- | Decoded value
  CasperT m a
decodeFile = decodeFileWith (liftIO . BS.readFile) (liftIO . doesFileExist)

getStoreMeta :: MonadIO m => CasperT m StoreMeta
getStoreMeta = decodeFile storeMetaPath StoreMetaMissing StoreMetaCorrupt

writeStoreMeta' :: MonadIO m => StoreMeta -> Store -> m ()
writeStoreMeta' meta store =
  let path = storeMetaPath store
   in liftIO $ BS.writeFile path (encode meta)

writeStoreMeta :: MonadIO m => StoreMeta -> CasperT m ()
writeStoreMeta meta = CasperT . lift . ReaderT $ \store -> do
  writeStoreMeta' meta store

getMeta :: MonadIO m => SHA256 -> CasperT m ContentMeta
getMeta sha = decodeFile ((<.> "meta") . blobPath sha) MetaMissing MetaCorrupt

retrieve :: (Content a, MonadIO m) => Address a -> CasperT m a
retrieve (Address sha) = decodeFile (blobPath sha) FileMissing FileCorrupt

roots :: MonadIO m => CasperT m (Set SHA256)
roots = storeRoots <$> getStoreMeta

blobPath :: SHA256 -> Store -> FilePath
blobPath (SHA256 sha) (Store root) = root </> "blob" </> Char8.unpack sha

metaPath :: SHA256 -> Store -> FilePath
metaPath (SHA256 sha) (Store root) = root </> "meta" </> Char8.unpack sha

storeMetaPath :: Store -> FilePath
storeMetaPath (Store root) = root </> "index"

storeWith ::
  (Content a, Monad m) =>
  -- | File writer function
  (FilePath -> BS.ByteString -> m ()) ->
  -- | Value to store
  a ->
  -- | Address to stored value
  CasperT m (Address a)
storeWith writeFile a = do
  let bs = encode a
      sha = hashBS bs
      meta = mkMeta a
  CasperT (asks $ blobPath sha) >>= writeCasper bs
  CasperT (asks $ metaPath sha) >>= writeCasper (encode meta)
  pure $ Address sha
  where
    writeCasper content path = lift $ writeFile path content

--
-- TODO: Write and hash lazily
store :: (Content a, MonadIO m) => a -> CasperT m (Address a)
store = storeWith ((liftIO .) . BS.writeFile)

newtype Store = Store {storeRoot :: FilePath}

markRoot :: MonadIO m => Address a -> CasperT m ()
markRoot (Address sha) = do
  StoreMeta roots <- getStoreMeta
  writeStoreMeta $ StoreMeta (S.insert sha roots)

unmarkRoot :: MonadIO m => Address a -> CasperT m ()
unmarkRoot (Address sha) = do
  StoreMeta roots <- getStoreMeta
  writeStoreMeta $ StoreMeta (S.delete sha roots)

isRoot :: MonadIO m => Address a -> CasperT m Bool
isRoot (Address sha) = do
  StoreMeta roots <- getStoreMeta
  pure $ S.member sha roots

data StoreError

findStore :: FilePath -> IO Store
findStore = pure . Store

initStore :: FilePath -> IO Store
initStore path = do
  exists <- doesDirectoryExist path
  unless exists (createDirectory path)
  none <- emptyDirectory path
  unless none (throwIO (mkIOError alreadyExistsErrorType "Path to store is not empty" Nothing (Just path)))
  createDirectory (path </> "blob")
  createDirectory (path </> "meta")
  writeStoreMeta' (StoreMeta mempty) store
  pure store
  where
    emptyDirectory = fmap null . listDirectory
    store = Store path

runCasperT :: MonadIO m => Store -> CasperT m a -> m (Either CasperError a)
runCasperT store (CasperT action) = runReaderT (runExceptT action) store
