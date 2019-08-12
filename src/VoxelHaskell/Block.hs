module VoxelHaskell.Block where

import Graphics.Rendering.OpenGL as GL

data Block = Block
  { x :: Int
  , y :: Int
  , z :: Int
  , colour :: Color3 Float
  }

black :: Color3 Float
black = Color3 0 0 0

basePlane :: Int -> [Block]
basePlane size =
  [ Block x y z black
  | x <- [-size..size]
  , y <- [0]
  , z <- [-size..size]
  ]

blocks :: [Block]
blocks = basePlane 5
  <> [Block 0 1 0 black]
