module VoxelHaskell.Block where

import Graphics.Rendering.OpenGL as GL

data Block = Block
  { colour :: Color3 Float
  }

black :: Color3 Float
black = Color3 0 0 0
