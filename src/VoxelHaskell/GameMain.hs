module VoxelHaskell.GameMain where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.MultiState
import Data.HList.HList

import VoxelHaskell.Input
import VoxelHaskell.Player
import VoxelHaskell.Rendering
import VoxelHaskell.World

gameMain :: IO ()
gameMain = do
  initRendering
  makeWindow
  renderState <- initOGL
  void $ liftIO $ runMultiStateT
    (initialPlayer :+: initialWorld :+: renderState :+: HNil)
    (forever mainLoop)

mainLoop
  :: (MonadMultiState Player m, MonadMultiState World m
    , MonadMultiState RenderState m, MonadIO m)
  => m ()
mainLoop = do
  handleInput
  renderFrame
