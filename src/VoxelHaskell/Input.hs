module VoxelHaskell.Input where

import Control.Monad.Trans
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import VoxelHaskell.Player
import VoxelHaskell.STMState
import VoxelHaskell.World

moveScale :: Float
moveScale = 2

mouseSensitivity :: Float
mouseSensitivity = recip 100

handleInput :: (MonadState Player m, MonadGet World m, MonadIO m) => m ()
handleInput = do
  onPress (GLFW.CharKey ',') (movePlayer Forward moveScale) (movePlayer Forward 0)
  onPress (GLFW.CharKey ' ') jump (pure ())

  GL.Position posX posY <- liftIO (GL.get GLFW.mousePos)
  GLFW.mousePos $= (GL.Position 0 0)

  look (fromIntegral posX * mouseSensitivity) (fromIntegral posY * mouseSensitivity)

onPress :: MonadIO m => GLFW.Key -> m () -> m () -> m ()
onPress k ma mb =
  liftIO (GLFW.getKey k) >>= \case
    GLFW.Press -> ma
    GLFW.Release -> mb
