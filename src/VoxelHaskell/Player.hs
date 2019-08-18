module VoxelHaskell.Player where

import Control.Lens
import Data.Fixed
import Linear

import VoxelHaskell.Physics
import VoxelHaskell.STMState
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
playerHeight = 10

jumpSpeed :: Float
jumpSpeed = 4

instance MonadGet World m => PhysicsObject Player m where
  tick :: Float -> Player -> m Player
  tick delta p = do
    let p2 = over vel (gravity delta) p
    let p3 = over pos (velocity delta (p ^. vel)) p2
    touchingGround p3 >>= \case
      False -> pure p3
      True -> pure $ set (vel . _y) 0 $ over (vel . _y) ((+ 0.5) . fromIntegral . round) $ p3

touchingGround :: MonadGet World m => Player -> m Bool
touchingGround player = do
  blockStanding <- getBlock (round <$> player ^. pos)
  pure $ case blockStanding of
    Nothing -> False
    Just _ -> True

initialPlayer :: Player
initialPlayer = Player
  { _pos = V3 20 (-10) 0
  , _vel = V3 0 0 0
  , _pitch = (-30)
  , _yaw = 0
  }

data Direction = Forward | Backward | DirLeft | DirRight | DirUp | DirDown

movePlayer :: MonadState Player m => Direction -> Float -> m ()
movePlayer Forward amount = do
  (Player _ _ pitch yaw) <- get
  modify (set (vel . _x) (amount * (sin yaw * cos pitch)))
  modify (set (vel . _z) (amount * (- (cos yaw * cos pitch))))
movePlayer DirUp amount = modify (over pos (+ (V3 0 amount 0)))
movePlayer DirDown amount = modify (over pos (+ (V3 0 (-amount) 0)))
movePlayer _ _ = pure ()

jump :: (MonadState Player m, MonadGet World m) => m ()
jump = do
  player <- get
  whenM (touchingGround player) $ modify (set (vel . _y) jumpSpeed)

look :: MonadState Player m => Float -> Float -> m ()
look dYaw dPitch = do
  modify (over yaw (flip mod' tau . (+ dYaw)))
  modify (over pitch (clamp (-tau/4) (tau/4) . (+ dPitch)))
