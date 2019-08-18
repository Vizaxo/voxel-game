module VoxelHaskell.Utils where

import Control.Monad
import Graphics.Rendering.OpenGL (Vector3(..))
import Linear

clamp :: Ord n => n -> n -> n -> n
clamp lower upper x
  | x <= lower = lower
  | x >= upper = upper
  | otherwise = x

type Radians = Float

tau :: Floating a => a
tau = 2*pi

vector3ToV3 :: Vector3 a -> V3 a
vector3ToV3 (Vector3 x y z) = V3 x y z

v3ToVector3 :: V3 a -> Vector3 a
v3ToVector3 (V3 x y z) = Vector3 x y z

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = mb >>= flip when ma

magnitudeSquared :: Num a => V3 a -> a
magnitudeSquared (V3 x y z) = x * x + y * y + z * z
