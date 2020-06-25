{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Internal where

import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Set (Set)
import qualified Data.Set as S

newtype CasperT m a = CasperT {unCasperT :: ReaderT Store m a}
  deriving (Functor, Applicative, Monad, MonadIO)

data SHA256

instance Eq SHA256

instance Ord SHA256

class Serialize a => Content a where
  references :: forall hash. (forall b. Address b -> hash) -> a -> [hash]

closure :: Monad m => Address a -> CasperT m (Set SHA256)
closure addr = go mempty [forget addr]
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

reserveds :: CasperT m [SHA256]
reserveds = undefined

toPath :: Store -> SHA256 -> FilePath
toPath = undefined

write :: Store -> SHA256 -> BS.ByteString -> IO ()
write = undefined
  where
    makeDirectory :: FilePath -> IO ()
    makeDirectory = undefined

data Store
