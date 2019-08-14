module VoxelHaskell.WorldGenerator where

import Graphics.Rendering.OpenGL (Vector3(..))

import VoxelHaskell.Block

generateWorld :: Vector3 Int -> Block
generateWorld (Vector3 x y z) = if y >= 0 then air else (if x >= z then red else blue)
