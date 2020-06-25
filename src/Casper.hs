module Casper where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Internal

serialize :: Content a => a -> BS.ByteString
serialize = undefined

deserialize :: Content a => BS.ByteString -> Maybe a
deserialize = undefined

store :: (Content a, MonadIO m) => a -> CasperT m (Address a)
store = undefined

data RetrievalError = Corruption | Missing

retrieve :: (Content a, MonadIO m) => Address a -> CasperT m (Either RetrievalError a)
retrieve = undefined

reserve :: Address a -> CasperT m ()
reserve = undefined

unreserve :: Address a -> CasperT m ()
unreserve = undefined

isReserved :: Address a -> CasperT m Bool
isReserved = undefined

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
