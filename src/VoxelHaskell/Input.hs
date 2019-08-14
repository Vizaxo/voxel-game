module VoxelHaskell.Input where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.MultiState
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import VoxelHaskell.Player
import VoxelHaskell.Utils

handleInput :: (MonadMultiState Player m, MonadIO m) => m ()
handleInput = do
  onPress (GLFW.CharKey ',') $ movePlayer Forward 0.1
  onPress (GLFW.CharKey 'O') $ movePlayer Backward 0.1
  onPress (GLFW.CharKey 'A') $ movePlayer DirLeft 0.1
  onPress (GLFW.CharKey 'E') $ movePlayer DirRight 0.1

  GL.Position posX posY <- liftIO (GL.get GLFW.mousePos)
  mModify (over angle (+ (fromIntegral posX / 7)))
  GLFW.mousePos $= (GL.Position 0 0)

onPress :: MonadIO m => GLFW.Key -> m () -> m ()
onPress k ma =
  liftIO (GLFW.getKey k) >>= \case
    GLFW.Press -> ma
    GLFW.Release -> pure ()
