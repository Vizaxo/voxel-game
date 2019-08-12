module VoxelHaskell.GameMain where

import Control.Lens
import Control.Monad.State

import VoxelHaskell.GameState
import VoxelHaskell.Input
import VoxelHaskell.Rendering

gameMain :: IO ()
gameMain = do
  initRendering
  makeWindow
  renderState <- initOGL
  void $ liftIO $ runStateT (forever mainLoop) (initialGameState, renderState)

mainLoop :: (MonadState (GameState, RenderState) m, MonadIO m) => m ()
mainLoop = do
  (gameState, _) <- get
  gameState' <- execStateT handleInput gameState
  modify (set _1 gameState')
  renderFrame
