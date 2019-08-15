module VoxelHaskell.Input where

import Control.Monad.Trans
import Control.Monad.Trans.MultiState
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import VoxelHaskell.Player

moveScale :: Float
moveScale = 0.2

mouseSensitivity :: Float
mouseSensitivity = recip 100

handleInput :: (MonadMultiState Player m, MonadIO m) => m ()
handleInput = do
  onPress (GLFW.CharKey ',') $ movePlayer Forward moveScale
  onPress (GLFW.CharKey 'O') $ movePlayer Backward moveScale
  onPress (GLFW.CharKey 'A') $ movePlayer DirLeft moveScale
  onPress (GLFW.CharKey 'E') $ movePlayer DirRight moveScale
  onPress (GLFW.CharKey ' ') $ movePlayer DirUp moveScale
  onPress (GLFW.SpecialKey GLFW.LSHIFT) $ movePlayer DirDown moveScale

  GL.Position posX posY <- liftIO (GL.get GLFW.mousePos)
  GLFW.mousePos $= (GL.Position 0 0)

  look (fromIntegral posX * mouseSensitivity) (fromIntegral posY * mouseSensitivity)

onPress :: MonadIO m => GLFW.Key -> m () -> m ()
onPress k ma =
  liftIO (GLFW.getKey k) >>= \case
    GLFW.Press -> ma
    GLFW.Release -> pure ()
