module VoxelHaskell.Rendering.Window where

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

  renderBlock (Block 0 0 0 (Color3 1 0 0))
  renderBlock (Block 1 0 0 (Color3 0 1 0))

  GLFW.swapBuffers

data Block = Block
  { x :: Float
  , y :: Float
  , z :: Float
  , colour :: Color3 Float
  }

renderBlock :: Block -> IO ()
renderBlock (Block x y z rgb) = do
  GL.renderPrimitive GL.Quads $ do
    let vertex3f x y z = vertex $ Vertex3 x y z
    color rgb
    vertex3f (x - 0.5) (y - 0.5) z
    vertex3f (x - 0.5) (y + 0.5) z
    vertex3f (x + 0.5) (y + 0.5) z
    vertex3f (x + 0.5) (y - 0.5) z

    vertex3f (x - 0.5) (y - 0.5) z
    vertex3f (x - 0.5) (y + 0.5) z
    vertex3f (x + 0.5) (y + 0.5) z
    vertex3f (x + 0.5) (y - 0.5) z
