module VoxelHaskell.GameMain where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock

import VoxelHaskell.Input
import VoxelHaskell.Physics
import VoxelHaskell.Player
import VoxelHaskell.Rendering
import VoxelHaskell.STMState
import VoxelHaskell.World
import VoxelHaskell.WorldGenerator

gameMain :: IO ()
gameMain = do
  initRendering
  makeWindow
  renderState <- initOGL
  let seed = 33
  world <- newTVarIO (mkWorld (generateWorld seed))
  player <- newTVarIO initialPlayer
  renderStateTVar <- newTVarIO renderState
  time <- liftIO getCurrentTime

  -- Mesh generation thread
  liftIO $ forkIO $ void
    $ runSTMStateT world
    $ runSTMStateT player
    $ runSTMStateT renderStateTVar
    $ forever generateMesh

  void
    $ runSTMStateT' (0 :: Int)
    $ runSTMStateT' time
    $ runSTMStateT world
    $ runSTMStateT player
    $ runSTMStateT renderStateTVar
    $ forever mainLoop

mainLoop
  :: (MonadState Player m, MonadState World m
    , MonadState RenderState m, MonadState Int m
    , MonadState UTCTime m, MonadIO m)
  => m ()
mainLoop = do
  handleInput
  renderFrame
  modifyM @Player (tick 0.05)
  displayFrameTime

displayFrameTime :: (MonadState UTCTime m, MonadIO m) => m ()
displayFrameTime = do
  lastTime <- get
  currTime <- liftIO getCurrentTime
  put currTime
  let diff = diffUTCTime currTime lastTime
  liftIO $ print diff
