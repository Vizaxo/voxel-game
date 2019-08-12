module VoxelHaskell.GameState where

import Control.Lens.TH
import Graphics.Rendering.OpenGL as GL

import VoxelHaskell.World

data GameState = GameState
  { _playerPos :: Vector3 Float
  , _playerAngle :: Float
  , _world :: World
  }
makeLenses ''GameState

initialGameState :: GameState
initialGameState = GameState
  { _playerPos = Vector3 0 3 10
  , _playerAngle = 0
  , _world = initialWorld
  }
