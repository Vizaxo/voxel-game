module VoxelHaskell.Rendering.Window where

import Control.Monad
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

makeWindow :: IO ()
makeWindow = do
  GLFW.openWindow (GL.Size 400 400) [] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"

mainLoop :: IO ()
mainLoop = do
  GL.clearColor $= Color4 0 0 0 0
  GL.clear [GL.ColorBuffer]

  GL.renderPrimitive GL.Triangles $
     mapM_ (\(x, y, z) -> vertex $ GL.Vertex3 x y z) myPoints

  GLFW.swapBuffers

myPoints :: [(GL.GLfloat, GL.GLfloat, GL.GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]
