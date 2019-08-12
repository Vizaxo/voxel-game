module VoxelHaskell.GameState where

import Control.Lens.TH
import Graphics.Rendering.OpenGL as GL

data GameState = GameState
  { _playerPos :: Vector3 Float
  , _playerAngle :: Float
  }
makeLenses ''GameState

initialGameState :: GameState
initialGameState = GameState
  { _playerPos = Vector3 0 3 10
  , _playerAngle = 0
  }
