module VoxelHaskell.STMState where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State

newtype STMState s a = STMState (ReaderT (TVar s) IO a)
  deriving (Functor, Applicative, Monad)

instance MonadState s (STMState s) where
  get = STMState (lift . readTVarIO =<< ask)
  put s = STMState (lift . atomically . flip writeTVar s =<< ask)

instance MonadIO (STMState s) where
  liftIO ma = STMState (lift ma)

runSTMState :: TVar s -> STMState s a -> IO (a, s)
runSTMState t (STMState f) = do
  a <- runReaderT f t
  s <- readTVarIO t
  pure (a, s)
