module VoxelHaskell.Camera where

import Control.Lens
import Control.Monad.Trans.MultiState
import Linear

import VoxelHaskell.Player
import VoxelHaskell.Utils

viewMatrix :: MonadMultiGet Player m => m (M44 Float)
viewMatrix = do
  player <- mGet
  pure $ mkTransformation
    (axisAngle (V3 1 0 0) (player ^. pitch))
    (V3 0 0 0)
    !*! mkTransformation
    (axisAngle (V3 0 1 0) (player ^. yaw))
    (V3 0 0 0)
    !*! mkTransformation
    (axisAngle (V3 0 1 0) 0)
    (- (player ^. pos))

projectionMatrix :: M44 Float
projectionMatrix = perspective (80 / 360 * tau) (8/9) 0.1 100

cameraMatrix :: MonadMultiGet Player m => m (M44 Float)
cameraMatrix = (projectionMatrix !*!) <$> viewMatrix
