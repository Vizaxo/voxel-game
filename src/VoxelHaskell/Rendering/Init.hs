module VoxelHaskell.Rendering.Init where

import Control.Monad.Trans
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import VoxelHaskell.Rendering.Types

makeWindow :: IO ()
makeWindow = do
  GLFW.initialize
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8] GLFW.Window
  GLFW.windowTitle $= "Voxel game"

  -- Disable vsync
  GLFW.swapInterval $= 0

  GLFW.disableSpecial GLFW.MouseCursor
  GLFW.mousePos $= (GL.Position 0 0)

initOGL :: IO RenderState
initOGL = do
  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Just GL.Back
  GL.depthFunc $= Just GL.Less

  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  vbo <- GL.genObjectName
  shaderProg <- makeShaderProgram
  GL.currentProgram $= Just shaderProg
  pure (RenderState vao vbo shaderProg)

makeShader :: GL.Program -> GL.ShaderType -> FilePath -> IO ()
makeShader shaderProg shaderType fileName = do
  shader <- GL.createShader shaderType
  source <- T.readFile ("resources/shaders/" <> fileName)
  GL.shaderSourceBS shader $= T.encodeUtf8 source
  GL.compileShader shader
  GL.get (GL.shaderInfoLog shader) >>= liftIO . print
  GL.attachShader shaderProg shader

makeShaderProgram :: IO GL.Program
makeShaderProgram = do
  shaderProg <- GL.createProgram

  makeShader shaderProg GL.VertexShader "colour.vert"
  makeShader shaderProg GL.GeometryShader "cube.geom"
  makeShader shaderProg GL.FragmentShader "colour.frag"

  GL.linkProgram shaderProg
  GL.get (GL.programInfoLog shaderProg) >>= liftIO . print
  pure shaderProg
