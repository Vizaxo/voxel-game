module VoxelHaskell.Rendering.Frame where

import Control.Monad.Trans
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Distributive (distribute)
import Data.Word
import Graphics.Rendering.OpenGL (Color4(..), ($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Foreign (sizeOf, nullPtr, castPtr, plusPtr, with)

import VoxelHaskell.Camera
import VoxelHaskell.Player
import VoxelHaskell.Rendering.Mesh
import VoxelHaskell.Rendering.Types
import VoxelHaskell.STMState

renderFrame
  :: (MonadGet Player m, MonadGet RenderState m, MonadGet MeshCache m
    , MonadIO m) => m ()
renderFrame = do
  renderState <- get

  GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  vertices <- getWorldMesh
  let numVertices = BS.length vertices `div` vertexSize

  GL.bindVertexArrayObject $= Just (renderState ^. vao)
  GL.bindBuffer GL.ArrayBuffer $= Just (renderState ^. vbo)
  liftIO $ BS.unsafeUseAsCString vertices $ \vs -> GL.bufferData GL.ArrayBuffer $=
    (fromIntegral $ BS.length vertices
    , vs
    , GL.DynamicDraw)

  let posAttribute = GL.AttribLocation 0
      colourAttribute = GL.AttribLocation 1
      faceBitmaskAttribute = GL.AttribLocation 2

  GL.vertexAttribPointer posAttribute $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral (8 * sizeOf (0 :: Float))) nullPtr)
  GL.vertexAttribArray posAttribute $= GL.Enabled

  GL.vertexAttribPointer colourAttribute $=
    (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (fromIntegral (8 * sizeOf (0 :: Float))) (plusPtr nullPtr (3 * sizeOf (0 :: Float))))
  GL.vertexAttribArray colourAttribute $= GL.Enabled

  GL.vertexAttribPointer faceBitmaskAttribute $=
    (GL.KeepIntegral, GL.VertexArrayDescriptor 1 GL.UnsignedInt (fromIntegral (8 * sizeOf (0 :: Word32))) (plusPtr nullPtr (7 * sizeOf (0 :: Float))))
  GL.vertexAttribArray faceBitmaskAttribute $= GL.Enabled

  cam <- cameraMatrix
  GL.currentProgram $= Just (renderState ^. shaderProg)

  GL.UniformLocation projectionLocation <- GL.get (GL.uniformLocation (renderState ^. shaderProg) "projection")
  liftIO $ with (distribute $ cam) $ \ptr ->
    GL.glUniformMatrix4fv projectionLocation 1 0 (castPtr ptr)

  liftIO $ GL.drawArrays GL.Points 0 (fromIntegral numVertices)
  liftIO $ GLFW.swapBuffers
