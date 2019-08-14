module VoxelHaskell.WorldGenerator where

import Data.Maybe
import Graphics.Rendering.OpenGL (Vector3(..))
import Math.Noise

import VoxelHaskell.Block

generateWorld :: Int -> Vector3 Int -> Block
generateWorld seed (Vector3 x y z) =
  if ground then checkerboard else air
  where
    ground = (fromIntegral y + heightScale) <= noiseAt seed heightScale x z
    checkerboard = if ((x + y + z) `mod` 2 == 0) then red else blue
    heightScale = 20

noiseAt :: Int -> Double -> Int -> Int -> Double
noiseAt seed heightScale x z =
  heightScale * (fromJust $ getValue perlin (fromIntegral x, 0, fromIntegral z))
  where
    frequency = recip 100
    lacunarity = 2
    octaves = 5
    persistence = 0.5
    perlin = Perlin frequency lacunarity octaves persistence seed
