module VoxelHaskell.Input where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.MultiState
import Data.Fixed
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import VoxelHaskell.Player
import VoxelHaskell.Utils

moveScale :: Float
moveScale = 0.2

handleInput :: (MonadMultiState Player m, MonadIO m) => m ()
handleInput = do
  onPress (GLFW.CharKey ',') $ movePlayer Forward moveScale
  onPress (GLFW.CharKey 'O') $ movePlayer Backward moveScale
  onPress (GLFW.CharKey 'A') $ movePlayer DirLeft moveScale
  onPress (GLFW.CharKey 'E') $ movePlayer DirRight moveScale

  GL.Position posX posY <- liftIO (GL.get GLFW.mousePos)
  mModify (over angleX (flip mod' 360 . (+ (fromIntegral posX / 7))))
  mModify (over angleY (clamp (-90) 90 . (+ (fromIntegral posY / 7))))
  GLFW.mousePos $= (GL.Position 0 0)

onPress :: MonadIO m => GLFW.Key -> m () -> m ()
onPress k ma =
  liftIO (GLFW.getKey k) >>= \case
    GLFW.Press -> ma
    GLFW.Release -> pure ()
