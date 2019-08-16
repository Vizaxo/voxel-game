module VoxelHaskell.Player where

import Control.Lens
import Control.Monad.Trans.MultiState
import Data.Fixed
import Linear

import VoxelHaskell.Physics
import VoxelHaskell.Utils

data Player = Player
  { _pos :: V3 Float
  , _vel :: V3 Float
  , _pitch :: Radians
  , _yaw :: Radians
  }
makeLenses ''Player

instance Monad m => PhysicsObject Player m where
  tick :: Float -> Player -> m Player
  tick delta p = do
    let p2 = over vel (gravity delta) p
    let p3 = over pos (velocity delta (p ^. vel)) p2
    pure p3


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
  let move = (* amount) <$> (V3 (sin yaw * cos pitch)
                              (- (sin pitch))
                              (- (cos yaw * cos pitch)))
  mModify (over pos (+ move))
movePlayer DirUp amount = mModify (over pos (+ (V3 0 amount 0)))
movePlayer DirDown amount = mModify (over pos (+ (V3 0 (-amount) 0)))
movePlayer _ _ = pure ()

look :: MonadMultiState Player m => Float -> Float -> m ()
look dYaw dPitch = do
  mModify (over yaw (flip mod' tau . (+ dYaw)))
  mModify (over pitch (clamp (-tau/4) (tau/4) . (+ dPitch)))
