module VoxelHaskell.GameMain where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock

import VoxelHaskell.Input
import VoxelHaskell.Physics
import VoxelHaskell.Player
import VoxelHaskell.Rendering.Frame
import VoxelHaskell.Rendering.Init
import VoxelHaskell.Rendering.Mesh
import VoxelHaskell.Rendering.Types
import VoxelHaskell.STMState
import VoxelHaskell.World
import VoxelHaskell.WorldGenerator

gameMain :: IO ()
gameMain = do
  makeWindow
  renderState <- initOGL
  let seed = 33
  world <- newTVarIO (mkWorld (generateWorld seed))
  player <- newTVarIO initialPlayer
  renderStateTVar <- newTVarIO renderState
  meshCache <- newTVarIO emptyMeshCache
  time <- liftIO getCurrentTime

  -- Chunk vertex generation thread
  liftIO $ forkIO $ void
    $ runSTMStateT world
    $ runSTMStateT player
    $ runSTMStateT renderStateTVar
    $ runSTMStateT meshCache
    $ forever generateChunkVertices

  -- World mesh generation thread
  liftIO $ forkIO $ void
    $ runSTMStateT world
    $ runSTMStateT player
    $ runSTMStateT renderStateTVar
    $ runSTMStateT meshCache
    $ forever generateWorldMesh

  void
    $ runSTMStateT' (0 :: Int)
    $ runSTMStateT' time
    $ runSTMStateT world
    $ runSTMStateT player
    $ runSTMStateT renderStateTVar
    $ runSTMStateT meshCache
    $ forever mainLoop

mainLoop
  :: (MonadState Player m, MonadState World m
    , MonadState RenderState m, MonadState Int m
    , MonadGet MeshCache m
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
