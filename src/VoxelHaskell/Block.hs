module VoxelHaskell.Block where

import Graphics.Rendering.OpenGL as GL

data Block' = Block
  { colour :: Color4 Float
  }

type Block = Maybe Block'

black :: Block
black = Just (Block (Color4 0 0 0 1))

red :: Block
red = Just (Block (Color4 1 0 0 1))

blue :: Block
blue = Just (Block (Color4 0 0 1 1))

air :: Block
air = Nothing
