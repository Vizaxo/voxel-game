module VoxelHaskell.Physics where

import Control.Lens
import Linear

class PhysicsObject o m where
  tick :: Float -> o -> m o

g :: Float
g = 9.81

gravity :: Float -> V3 Float -> V3 Float
gravity delta = over _y (subtract (g * delta))

velocity :: Float -> V3 Float -> V3 Float -> V3 Float
velocity delta (V3 xv yv zv) (V3 x y z) = V3 x' y' z'
  where
    x' = x + xv * delta
    y' = y + yv * delta
    z' = z + zv * delta
