module Util where

import Control.Concurrent.STM
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)

class Monad m => MonadSTM m where liftSTM :: STM a -> m a

instance MonadSTM STM where liftSTM = id

instance MonadSTM m => MonadSTM (ReaderT r m) where liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (StateT s m) where liftSTM = lift . liftSTM
