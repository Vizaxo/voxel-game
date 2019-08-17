module VoxelHaskell.STMState where

import Control.Concurrent.STM
import Control.Monad.Reader

newtype STMStateT s m a = STMStateT (ReaderT (TVar s) m a)
  deriving (Functor, Applicative, Monad)

class Monad m => MonadGet s m where
  get :: m s

class Monad m => MonadPut s m where
  put :: s -> m ()

class (MonadGet s m, MonadPut s m) => MonadState s m where
  modify :: (s -> s) -> m ()

-- Note: not atomic. Might cause thread race issues.
modifyM :: forall s m. MonadState s m => (s -> m s) -> m ()
modifyM mf = do
  s <- get
  put =<< mf s

instance {-# OVERLAPS #-} MonadIO m => MonadGet s (STMStateT s m) where
  get = STMStateT (liftIO . readTVarIO =<< ask)

instance {-# OVERLAPS #-} MonadIO m => MonadPut s (STMStateT s m) where
  put s = STMStateT (liftIO . atomically . flip writeTVar s =<< ask)

instance {-# OVERLAPS #-} MonadIO m => MonadState s (STMStateT s m) where
  modify f = STMStateT (do tvar <- ask
                           liftIO $ atomically $ do
                             s <- readTVar tvar
                             writeTVar tvar (f s))

instance {-# OVERLAPPABLE #-} (MonadGet s m, Monad m) => MonadGet s (STMStateT s' m) where
  get = STMStateT (lift get)

instance {-# OVERLAPPABLE #-} (MonadPut s m, Monad m) => MonadPut s (STMStateT s' m) where
  put s = STMStateT (lift (put s))

instance {-# OVERLAPPABLE #-} (MonadState s m, Monad m) => MonadState s (STMStateT s' m) where
  modify f = STMStateT (lift (modify f))

instance MonadIO m => MonadIO (STMStateT s m) where
  liftIO ma = STMStateT (liftIO ma)

instance MonadTrans (STMStateT s) where
  lift ma = STMStateT (lift ma)

runSTMStateT :: MonadIO m => TVar s -> STMStateT s m a -> m (a, s)
runSTMStateT t (STMStateT f) = do
  a <- runReaderT f t
  s <- liftIO $ readTVarIO t
  pure (a, s)

runSTMStateT' :: MonadIO m => s -> STMStateT s m a -> m (a, s)
runSTMStateT' s (STMStateT f) = do
  t <- liftIO $ newTVarIO s
  a <- runReaderT f t
  s <- liftIO $ readTVarIO t
  pure (a, s)
