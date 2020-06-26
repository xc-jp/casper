{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Internal where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.Foldable (fold)
import Data.Serialize
import Data.Set (Set)
import qualified Data.Set as S
import System.Directory
import System.FilePath

data CasperError
  = FileCorrupt FilePath String
  | FileMissing FilePath
  | MetaCorrupt FilePath String
  | MetaMissing FilePath
  | StoreMetaCorrupt FilePath String
  | StoreMetaMissing FilePath

newtype CasperT m a = CasperT {unCasperT :: ExceptT CasperError (ReaderT Store m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CasperError)

data SHA256

instance Eq SHA256

instance Ord SHA256

instance Serialize SHA256 where
  get = undefined
  put = undefined

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
hashBS = undefined

delete :: Monad m => SHA256 -> CasperT m ()
delete sha = do
  path <- CasperT $ asks $ shaPath sha
  undefined

-- future optimization: traverse shared deps only once
collectGarbage :: MonadIO m => CasperT m ()
collectGarbage = do
  rs <- roots
  everything <- getEverything
  notGarbage <- closure (S.toList rs)
  forM_ (S.toList $ everything S.\\ notGarbage) delete
  where
    getEverything :: CasperT m (Set SHA256)
    getEverything = undefined

newtype StoreMeta = StoreMeta {storeRoots :: Set SHA256}
  deriving (Serialize)

decodeFile ::
  (Serialize a, MonadIO m) =>
  (Store -> FilePath) ->
  (FilePath -> CasperError) ->
  (FilePath -> String -> CasperError) ->
  CasperT m a
decodeFile getPath eMissing eCorrupt = do
  path <- CasperT $ asks getPath
  exists <- liftIO $ doesFileExist path
  unless exists $ throwError (eMissing path)
  bs <- liftIO $ BS.readFile path
  case decode bs of
    Right a -> pure a
    Left err -> throwError (eCorrupt path err)

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
shaPath = undefined

storeMetaPath :: Store -> FilePath
storeMetaPath (Store root) = root </> "meta"

-- TODO: Write and hash lazily
store :: (Content a, MonadIO m) => a -> CasperT m (Address a)
store a = do
  let bs = encode a
      sha = hashBS bs
      meta = mkMeta a
  path <- CasperT $ asks $ shaPath sha
  liftIO $ do
    createDirectoryIfMissing True (dropFileName path)
    BS.writeFile path bs
    BS.writeFile (path <.> "meta") (encode meta)
  pure $ Address sha

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

initStore :: FilePath -> IO (Either InitError Store)
initStore = undefined

runCasperT :: MonadIO m => Store -> CasperT m a -> m a
runCasperT = undefined
