{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Internal where

import Control.Exception (Exception, handle, throwIO)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (fold)
import Data.Serialize
import Data.Set (Set)
import qualified Data.Set as S
import System.Directory
import System.FilePath
import System.IO.Error (isAlreadyExistsError)

data CasperError
  = FileCorrupt FilePath String
  | FileMissing FilePath
  | MetaCorrupt FilePath String
  | MetaMissing FilePath
  | StoreMetaCorrupt FilePath String
  | StoreMetaMissing FilePath
  deriving (Show, Eq)

-- TODO(considerate): Don't depend on Exception. Currently required for initStore
instance Exception CasperError

newtype CasperT m a = CasperT {unCasperT :: ExceptT CasperError (ReaderT Store m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CasperError)

instance MonadTrans CasperT where
  lift = CasperT . lift . lift

newtype SHA256 = SHA256 {unSHA256 :: BS.ByteString}
  deriving (Eq, Ord)

instance Serialize SHA256 where
  get = SHA256 <$> get
  put = put . unSHA256

class Serialize a => Content a where
  references ::
    forall ref. (forall b. Address b -> ref) -> a -> [ref]

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

newtype Address a = Address {forget :: SHA256}
  deriving (Eq, Ord)

mkMeta :: Content a => a -> ContentMeta
mkMeta a = ContentMeta (S.fromList $ references' a)

hashBS :: BS.ByteString -> SHA256
hashBS = SHA256 . SHA256.hash

delete :: MonadIO m => SHA256 -> CasperT m ()
delete sha = do
  path <- CasperT $ asks $ shaPath sha
  liftIO $ removeFile path
  liftIO $ removeFile (path <.> "meta")

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
      -- need to special case meta files since they don't map to SHA256s directly
      let plain = filter (not . hasExtension) paths
      either throwError (pure . S.fromList) (traverse (decodeSha root) plain)
      where
        decodeSha :: FilePath -> String -> Either CasperError SHA256
        decodeSha root name = case Base16.decode (Char8.pack name) of
          (sha, rest) | BS.length sha == 32 && BS.null rest -> Right (SHA256 sha)
          _ -> Left (FileCorrupt (root </> name) "File name does not map to a valid sha256 hash")

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

writeStoreMeta :: MonadIO m => StoreMeta -> CasperT m ()
writeStoreMeta meta = do
  path <- CasperT $ asks storeMetaPath
  liftIO $ BS.writeFile path (encode meta)

getMeta :: MonadIO m => SHA256 -> CasperT m ContentMeta
getMeta sha = decodeFile ((<.> "meta") . shaPath sha) MetaMissing MetaCorrupt

retrieve :: (Content a, MonadIO m) => Address a -> CasperT m a
retrieve (Address sha) = decodeFile (shaPath sha) FileMissing FileCorrupt

roots :: MonadIO m => CasperT m (Set SHA256)
roots = storeRoots <$> getStoreMeta

shaPath :: SHA256 -> Store -> FilePath
shaPath (SHA256 sha) (Store root) = root </> Char8.unpack (Base16.encode sha)

storeMetaPath :: Store -> FilePath
storeMetaPath (Store root) = root </> "root.meta"

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
  path <- CasperT $ asks $ shaPath sha
  lift $ do
    writeFile path bs
    writeFile (path <.> "meta") (encode meta)
  pure $ Address sha

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

findStore :: FilePath -> IO (Either StoreError Store)
findStore =
  -- First check if appropriate meta-file* is present
  --   * contains list(?) of roots
  undefined

data InitError = PathExists | CannotCreate
  deriving (Show, Eq)

initStore :: FilePath -> IO (Either InitError Store)
initStore path = handle (pure . Left . handler) $ do
  createDirectory path -- create store path
  result <- runCasperT store $ writeStoreMeta $ StoreMeta mempty -- create store meta file
  either throwIO (const (pure $ Right store)) result
  pure (Right store)
  where
    store = Store path
    handler :: IOError -> InitError
    handler err
      | isAlreadyExistsError err = PathExists
      | otherwise = CannotCreate

runCasperT :: MonadIO m => Store -> CasperT m a -> m (Either CasperError a)
runCasperT store (CasperT action) = runReaderT (runExceptT action) store
