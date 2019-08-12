module VoxelHaskell.GameMain where

import Control.Monad.State

import VoxelHaskell.GameState
import VoxelHaskell.Input
import VoxelHaskell.Rendering

gameMain :: IO ()
gameMain = do
  initRendering
  makeWindow
  game

game :: IO ()
game = void $ runStateT (forever mainLoop) initialGameState

mainLoop :: (MonadState GameState m, MonadIO m) => m ()
mainLoop = do
  handleInput
  renderFrame
