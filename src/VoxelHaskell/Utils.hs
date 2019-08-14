module VoxelHaskell.Utils where

import Control.Monad.Trans.MultiState

mModify :: MonadMultiState s m => (s -> s) -> m ()
mModify f = do
  s <- mGet
  mSet (f s)
