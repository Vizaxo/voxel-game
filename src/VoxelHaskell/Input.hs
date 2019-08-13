module VoxelHaskell.Input where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), ($=))
import qualified Graphics.UI.GLFW as GLFW

import VoxelHaskell.GameState

handleInput :: (MonadState GameState m, MonadIO m) => m ()
handleInput = do
  onPress (GLFW.CharKey ',')
    $ modify (over playerPos (liftA2 (+) (Vector3 0 0 (-0.1))))
  onPress (GLFW.CharKey 'O')
    $ modify (over playerPos (liftA2 (+) (Vector3 0 0 0.1)))
  onPress (GLFW.CharKey 'A')
    $ modify (over playerPos (liftA2 (+) (Vector3 (-0.1) 0 0)))
  onPress (GLFW.CharKey 'E')
    $ modify (over playerPos (liftA2 (+) (Vector3 (0.1) 0 0)))

  GL.Position posX posY <- liftIO (GL.get GLFW.mousePos)
  modify (over playerAngle (+ (fromIntegral posX / 7)))
  GLFW.mousePos $= (GL.Position 0 0)

onPress :: MonadIO m => GLFW.Key -> m () -> m ()
onPress k ma =
  liftIO (GLFW.getKey k) >>= \case
    GLFW.Press -> ma
    GLFW.Release -> pure ()
