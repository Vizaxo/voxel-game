module VoxelHaskell.Rendering.Window where

import Control.Monad
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

makeWindow :: IO ()
makeWindow = do
  GLFW.openWindow (GL.Size 400 400) [] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"

  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

  forever (GLFW.swapBuffers)
