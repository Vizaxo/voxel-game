module VoxelHaskell.Camera where

import Control.Lens
import Linear

import VoxelHaskell.Player
import VoxelHaskell.STMState
import VoxelHaskell.Utils

playerToCamera :: V3 Float
playerToCamera = V3 0 1.5 0

viewMatrix :: MonadGet Player m => m (M44 Float)
viewMatrix = do
  player <- get
  pure $ mkTransformation
    (axisAngle (V3 1 0 0) (player ^. pitch))
    (V3 0 0 0)
    !*! mkTransformation
    (axisAngle (V3 0 1 0) (player ^. yaw))
    (V3 0 0 0)
    !*! mkTransformation
    (axisAngle (V3 0 1 0) 0)
    (- (player ^. pos + playerToCamera))

projectionMatrix :: M44 Float
projectionMatrix = perspective (80 / 360 * tau) (8/9) 0.1 100

cameraMatrix :: MonadGet Player m => m (M44 Float)
cameraMatrix = (projectionMatrix !*!) <$> viewMatrix
