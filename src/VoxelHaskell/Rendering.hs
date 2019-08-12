module VoxelHaskell.Rendering where

import Control.Monad.State
import Control.Lens
import qualified Data.Map as M
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color3(..), Vertex3(..), Color4(..), ($=))
import qualified Graphics.UI.GLFW as GLFW

import VoxelHaskell.Block
import VoxelHaskell.GameState
import VoxelHaskell.World

initRendering :: IO ()
initRendering = void $ GLFW.initialize

makeWindow :: IO ()
makeWindow = do
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"

  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Just GL.Back
  GL.depthFunc $= Just GL.Less

  GLFW.disableSpecial GLFW.MouseCursor
  GLFW.mousePos $= (GL.Position 0 0)

renderFrame :: (MonadState GameState m, MonadIO m) => m ()
renderFrame = do
  state <- get
  liftIO $ GL.loadIdentity
  liftIO $ GL.perspective 70 1 0.1 100
  liftIO $ GL.rotate (state ^. playerAngle) (Vector3 0 1 (0 :: Float))
  liftIO $ GL.translate (negate <$> state ^. playerPos)

  liftIO $ GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  renderWorld (state ^. world)
  --mapM_ (liftIO . renderBlock) blocks

  liftIO GLFW.swapBuffers

toFloat :: Integral n => n -> Float
toFloat = fromIntegral

renderWorld :: MonadIO m => World -> m ()
renderWorld (World (M.toList -> chunks)) = flip mapM_ chunks $ \(pos, chunk) ->
  liftIO $ GL.preservingMatrix $ do
    GL.translate ((toFloat . (*16)) <$> pos)
    renderChunk chunk

renderChunk :: MonadIO m => Chunk -> m ()
renderChunk (Chunk (M.toList -> blocks)) = flip mapM_ blocks $ \(pos, block) ->
  liftIO $ renderBlock pos block

renderBlock :: Vector3 Int -> Block -> IO ()
renderBlock (Vector3 (toFloat -> x) (toFloat -> y) (toFloat -> z)) (Block rgb) = do
  GL.renderPrimitive GL.Quads $ do
    let vertex3f x y z = GL.vertex $ Vertex3 x y z
    GL.preservingMatrix $ do
      GL.color (Color3 (0 :: Float) 1 0)
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z + 0.5)

      GL.color (Color3 (1 :: Float) 0 0)
      vertex3f (x - 0.5) (y - 0.5) (z - 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z - 0.5)

      GL.color (Color3 (0 :: Float) 0 1)
      vertex3f (x - 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z - 0.5)

      GL.color (Color3 (0.5 :: Float) 0 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)

      --bottom
      GL.color (Color3 (0.5 :: Float) 0.5 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z - 0.5)

      --top
      GL.color (Color3 (1 :: Float) 0.5 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
