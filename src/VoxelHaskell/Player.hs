module VoxelHaskell.Player where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.MultiState
import Graphics.Rendering.OpenGL as GL

import VoxelHaskell.Utils

data Player = Player
  { _pos :: Vector3 Float
  , _angleX :: Float
  , _angleY :: Float
  }
makeLenses ''Player

initialPlayer :: Player
initialPlayer = Player
  { _pos = Vector3 0 3 0
  , _angleX = 180
  , _angleY = 0
  }

data Direction = Forward | Backward | DirLeft | DirRight | DirUp | DirDown

movePlayer :: MonadMultiState Player m => Direction -> Float -> m ()
movePlayer dir amount = mModify (over pos (liftA2 (+) vect))
  where vect = case dir of
          Forward -> Vector3 0 0 amount
          Backward -> Vector3 0 0 (-amount)
          DirRight -> Vector3 (-amount) 0 0
          DirLeft -> Vector3 amount 0 0
          DirUp -> Vector3 0 amount 0
          DirDown -> Vector3 0 (-amount) 0
