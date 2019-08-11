module VoxelHaskell.Rendering.Window where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

makeWindow :: IO ()
makeWindow = do
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"

  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= (Nothing)

  GL.depthFunc $= Just Less
  GL.loadIdentity
  GL.perspective 70 1 0.1 10
  GL.translate (Vector3 0 0 (-2 ::Float))

mainLoop :: IO ()
mainLoop = do
  GL.clearColor $= Color4 0 0 0 0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.rotate (-1) $ Vector3 0 1 (0 :: Float)

  preservingMatrix $ do
    GL.scale (0.3 :: Float) 0.3 0.3
    GL.rotate (-30) $ Vector3 1 0 (0 :: Float)
    renderBlock (Block 0 0 0 (Color3 1 0 0))
    GL.translate (Vector3 1 1 (1::Float))
    renderBlock (Block 0 0 0 (Color3 1 0 0))

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
      -- front
    preservingMatrix $ do
      color (Color3 (0 :: Float) 1 0)
      vertex3f (x - 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)

      color (Color3 (1 :: Float) 0 0)
      vertex3f (x - 0.5) (y - 0.5) (z - 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z - 0.5)

      color (Color3 (0 :: Float) 0 1)
      vertex3f (x - 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z - 0.5)

      color (Color3 (0.5 :: Float) 0 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z - 0.5)

      --bottom
      color (Color3 (1 :: Float) 0.5 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z - 0.5)

      --top
      color (Color3 (0.5 :: Float) 0.5 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z - 0.5)

renderSquare :: Float -> Float -> Float -> IO ()
renderSquare x y z = do
  let vertex3f x y z = vertex $ Vertex3 (x :: Float) y z
  --GL.rotate 30 (Vector3 (1 :: Float) 1 1)
  vertex3f (x - 0.5) (y - 0.5) z
  vertex3f (x - 0.5) (y + 0.5) z
  vertex3f (x + 0.5) (y + 0.5) z
  vertex3f (x + 0.5) (y - 0.5) z
