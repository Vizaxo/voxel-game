module VoxelHaskell.Utils where

import Control.Monad.Trans.MultiState

mModify :: MonadMultiState s m => (s -> s) -> m ()
mModify f = do
  s <- mGet
  mSet (f s)

mModifyM :: MonadMultiState s m => (s -> m s) -> m ()
mModifyM f = do
  s <- mGet
  mSet =<< (f s)

clamp :: Ord n => n -> n -> n -> n
clamp lower upper x
  | x <= lower = lower
  | x >= upper = upper
  | otherwise = x

type Radians = Float

tau :: Floating a => a
tau = 2*pi
