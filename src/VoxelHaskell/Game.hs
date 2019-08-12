module VoxelHaskell.Game where

import Control.Lens.TH
import Graphics.Rendering.OpenGL as GL

data GameState = GameState
  { _playerPos :: Vector3 Float
  , _playerAngle :: Float
  }
makeLenses ''GameState
