module VoxelHaskell.GameMain where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.MultiState
import Data.HList.HList

import VoxelHaskell.Input
import VoxelHaskell.Physics
import VoxelHaskell.Player
import VoxelHaskell.Rendering
import VoxelHaskell.Utils
import VoxelHaskell.World
import VoxelHaskell.WorldGenerator

gameMain :: IO ()
gameMain = do
  initRendering
  makeWindow
  renderState <- initOGL
  let seed = 33
  void $ liftIO $ runMultiStateT
    (initialPlayer :+: mkWorld (generateWorld seed) :+: renderState :+: HNil)
    (forever mainLoop)

mainLoop
  :: (MonadMultiState Player m, MonadMultiState World m
    , MonadMultiState RenderState m, MonadIO m)
  => m ()
mainLoop = do
  handleInput
  renderFrame
  mModifyM @Player (tick 0.01)
