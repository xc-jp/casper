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

delete :: Address a -> CasperT m ()
delete = undefined

data IndexStructure

-- (/>) :: Address a -> (a -> Address b) -> Address a -> CasperT m b

data StoreError

findStore :: FilePath -> IO (Either StoreError Store)
findStore = undefined

runCasperT :: MonadIO m => Store -> CasperT m a -> m a
runCasperT = undefined
