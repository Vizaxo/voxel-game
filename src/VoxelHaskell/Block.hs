module VoxelHaskell.Block where

import Graphics.Rendering.OpenGL as GL

data Block = Block
  { colour :: Color4 Float
  }

black :: Color4 Float
black = Color4 0 0 0 1
