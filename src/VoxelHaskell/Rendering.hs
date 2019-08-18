module VoxelHaskell.Rendering where

import Control.Monad
import Control.Monad.Trans
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Distributive (distribute)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Word
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color4(..), ($=))
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Foreign (sizeOf, nullPtr, castPtr, plusPtr, with)

import VoxelHaskell.Camera
import VoxelHaskell.Player
import VoxelHaskell.STMState
import VoxelHaskell.Mesh

vertexSize :: Int
vertexSize = sizeOf (undefined :: Vector3 Float) + sizeOf (undefined :: Color4 Float)
  + sizeOf (undefined :: FaceBitmask)

data RenderState = RenderState
  { _vao :: GL.VertexArrayObject
  , _vbo :: GL.BufferObject
  , _shaderProg :: GL.Program
  }
makeLenses ''RenderState

initRendering :: IO ()
initRendering = void $ GLFW.initialize

makeWindow :: IO ()
makeWindow = do
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GLFW.swapInterval $= 0

  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Just GL.Back
  GL.depthFunc $= Just GL.Less

  GLFW.disableSpecial GLFW.MouseCursor
  GLFW.mousePos $= (GL.Position 0 0)

initOGL :: IO RenderState
initOGL = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName

  GL.bindVertexArrayObject $= Just vao

  vertexShader <- GL.createShader GL.VertexShader
  geometryShader <- GL.createShader GL.GeometryShader
  fragmentShader <- GL.createShader GL.FragmentShader

  vertSource <- T.readFile "resources/shaders/colour.vert"
  GL.shaderSourceBS vertexShader $= T.encodeUtf8 vertSource

  geomSource <- T.readFile "resources/shaders/cube.geom"
  GL.shaderSourceBS geometryShader $= T.encodeUtf8 geomSource

  fragSource <- T.readFile "resources/shaders/colour.frag"
  GL.shaderSourceBS fragmentShader $= T.encodeUtf8 fragSource

  GL.compileShader vertexShader
  GL.get (GL.shaderInfoLog vertexShader) >>= liftIO . print
  GL.compileShader geometryShader
  GL.get (GL.shaderInfoLog geometryShader) >>= liftIO . print
  GL.compileShader fragmentShader
  GL.get (GL.shaderInfoLog fragmentShader) >>= liftIO . print


  shaderProg <- GL.createProgram

  GL.attachShader shaderProg vertexShader
  GL.attachShader shaderProg geometryShader
  GL.attachShader shaderProg fragmentShader
  GL.linkProgram shaderProg
  GL.get (GL.programInfoLog shaderProg) >>= liftIO . print
  GL.currentProgram $= Just shaderProg

  pure (RenderState vao vbo shaderProg)

renderFrame
  :: (MonadGet Player m, MonadGet RenderState m, MonadGet MeshCache m
    , MonadIO m) => m ()
renderFrame = do
  renderState <- get

  GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  vertices <- getMeshVertices
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
