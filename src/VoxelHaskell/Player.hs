module VoxelHaskell.Player where

import Control.Lens
import Control.Monad.Trans.MultiState
import Data.Fixed
import Linear

import VoxelHaskell.Physics
import VoxelHaskell.Utils
import VoxelHaskell.World

data Player = Player
  { _pos :: V3 Float
  , _vel :: V3 Float
  , _pitch :: Radians
  , _yaw :: Radians
  }
  deriving Show
makeLenses ''Player

playerHeight :: Float
playerHeight = 1.5

jumpSpeed :: Float
jumpSpeed = 4

instance MonadMultiGet World m => PhysicsObject Player m where
  tick :: Float -> Player -> m Player
  tick delta p = do
    let p2 = over vel (gravity delta) p
    let p3 = over pos (velocity delta (p ^. vel)) p2
    touchingGround p3 >>= \case
      False -> pure p3
      True -> pure $ set (vel . _y) 0 $ over (vel . _y) ((+ 0.5) . fromIntegral . round) $ p3

touchingGround :: MonadMultiGet World m => Player -> m Bool
touchingGround player = do
  blockStanding <- getBlock (round <$> player ^. pos)
  pure $ case blockStanding of
    Nothing -> False
    Just _ -> True

initialPlayer :: Player
initialPlayer = Player
  { _pos = V3 0 3 0
  , _vel = V3 0 0 0
  , _pitch = (-30)
  , _yaw = 0
  }

data Direction = Forward | Backward | DirLeft | DirRight | DirUp | DirDown

movePlayer :: MonadMultiState Player m => Direction -> Float -> m ()
movePlayer Forward amount = do
  (Player _ _ pitch yaw) <- mGet
  mModify (set (vel . _x) (amount * (sin yaw * cos pitch)))
  mModify (set (vel . _z) (amount * (- (cos yaw * cos pitch))))
movePlayer DirUp amount = mModify (over pos (+ (V3 0 amount 0)))
movePlayer DirDown amount = mModify (over pos (+ (V3 0 (-amount) 0)))
movePlayer _ _ = pure ()

jump :: (MonadMultiState Player m, MonadMultiGet World m) => m ()
jump = do
  player <- mGet
  whenM (touchingGround player) $ mModify (set (vel . _y) jumpSpeed)

look :: MonadMultiState Player m => Float -> Float -> m ()
look dYaw dPitch = do
  mModify (over yaw (flip mod' tau . (+ dYaw)))
  mModify (over pitch (clamp (-tau/4) (tau/4) . (+ dPitch)))
