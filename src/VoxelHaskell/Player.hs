module VoxelHaskell.Player where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.MultiState
import Graphics.Rendering.OpenGL as GL

import VoxelHaskell.Utils

data Player = Player
  { _pos :: Vector3 Float
  , _angle :: Float
  }
makeLenses ''Player

initialPlayer :: Player
initialPlayer = Player
  { _pos = Vector3 0 3 10
  , _angle = 180
  }

data Direction = Forward | Backward | DirLeft | DirRight

movePlayer :: MonadMultiState Player m => Direction -> Float -> m ()
movePlayer dir amount = mModify (over pos (liftA2 (+) vect))
  where vect = case dir of
          Forward -> Vector3 0 0 amount
          Backward -> Vector3 0 0 (-amount)
          DirRight -> Vector3 (-amount) 0 0
          DirLeft -> Vector3 amount 0 0
