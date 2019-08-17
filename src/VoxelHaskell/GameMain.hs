module VoxelHaskell.GameMain where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.MultiState
import Data.HList.HList
import Data.Time.Clock

import VoxelHaskell.Input
import VoxelHaskell.Physics
import VoxelHaskell.Player
import VoxelHaskell.Rendering
import VoxelHaskell.STMState
import VoxelHaskell.Utils
import VoxelHaskell.World
import VoxelHaskell.WorldGenerator

gameMain :: IO ()
gameMain = do
  initRendering
  makeWindow
  renderState <- initOGL
  tvar <- newTVarIO renderState
  let seed = 33
  time <- liftIO getCurrentTime
  void $ runSTMState tvar
    $ runMultiStateT ((0 :: Int) :+: (mkWorld (generateWorld seed))
                      :+: time :+: initialPlayer :+: HNil)
    $ forever (mainLoop tvar)

mainLoop
  :: (MonadMultiState Player m, MonadMultiState World m
    , MonadState RenderState m, MonadMultiState Int m
    , MonadMultiState UTCTime m, MonadIO m)
  => TVar RenderState -> m ()
mainLoop tvar = do
  -- Once every 50 frames, try to re-generate the world mesh
  counter <- mGet @Int
  when (counter == 10) $ do
    player <- mGet @Player
    world <- mGet @World
    mSet @Int 10
    liftIO $ print "Spawing generator thread"
    void $ liftIO $ forkIO (void $ runSTMState tvar $ runMultiStateT (player :+: world :+: HNil) generateMesh)
  mModify @Int (+1)
  when (counter == 10000) $ do
    mSet @Int 0
  handleInput
  renderFrame
  mModifyM @Player (tick 0.05)
  displayFrameTime

displayFrameTime :: (MonadMultiState UTCTime m, MonadIO m) => m ()
displayFrameTime = do
  lastTime <- mGet
  currTime <- liftIO getCurrentTime
  mSet currTime
  let diff = diffUTCTime currTime lastTime
  liftIO $ print diff
