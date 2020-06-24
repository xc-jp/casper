{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Internal where

import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Set (Set)

newtype CasperT m a = CasperT {unCasperT :: ReaderT Store m a}
  deriving (Functor, Applicative, Monad, MonadIO)

data SHA256

class Serialize a => Content a where
  references :: forall hash. (forall b. Address b -> hash) -> a -> [hash]

closure :: Address a -> CasperT m (Set SHA256)
closure = undefined
  where
    dependencies :: SHA256 -> CasperT m (Set SHA256)
    dependencies = undefined

newtype Address a = Address {forget :: SHA256}

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
