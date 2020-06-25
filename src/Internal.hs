{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Internal where

import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Foldable (fold)
import Data.Serialize
import Data.Set (Set)
import qualified Data.Set as S

newtype CasperT m a = CasperT {unCasperT :: ReaderT Store m a}
  deriving (Functor, Applicative, Monad, MonadIO)

data SHA256

instance Eq SHA256

instance Ord SHA256

instance Serialize SHA256 where
  get = undefined
  put = undefined

class Serialize a => Content a where
  references ::
    forall ref. (forall b. Address b -> ref) -> a -> [ref]

newtype ContentMeta = ContentMeta {deps :: (Set SHA256)}
  deriving (Serialize)

closure :: Monad m => SHA256 -> CasperT m (Set SHA256)
closure sha = go mempty [sha]
  where
    go checked [] = pure checked
    go checked (h : t)
      | S.member h checked = go checked t
      | otherwise = do
        transitive <- dependencies h
        go (S.insert h checked) (transitive <> t)
    dependencies :: SHA256 -> CasperT m [SHA256]
    dependencies = undefined

newtype Address a = Address {forget :: SHA256}
  deriving (Eq, Ord)

hash :: Content a => a -> SHA256
hash = undefined

allContent :: CasperT m (Set SHA256)
allContent = undefined

deleteContent :: SHA256 -> CasperT m ()
deleteContent = undefined

-- future optimization: traverse shared deps only once
collectGarbage :: Monad m => CasperT m ()
collectGarbage = do
  -- roots <- reserveds
  -- everything <- allContent
  -- notGarbage <- forM (S.toList roots) closure
  -- forM_ (S.toList $ everything S.\\ fold notGarbage) deleteContent
  undefined

newtype StoreMeta = StoreMeta {roots :: Set SHA256}
  deriving (Serialize)

data MetaError = MetaCorrupted | MetaMissing

getMetaFile :: CasperT m (Either MetaError StoreMeta)
getMetaFile = undefined

reserveds :: Monad m => CasperT m (Either MetaError (Set SHA256))
reserveds = (fmap . fmap) roots getMetaFile

shaPath :: SHA256 -> Store -> FilePath
shaPath = undefined

metaPath :: Store -> FilePath
metaPath = undefined

write :: Store -> SHA256 -> BS.ByteString -> IO ()
write = undefined
  where
    makeDirectory :: FilePath -> IO ()
    makeDirectory = undefined

newtype Store = Store {storeRoot :: FilePath}
