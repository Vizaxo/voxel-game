module VoxelHaskell.Rendering where

import Control.Applicative
import Control.Monad.State
import Control.Lens
import qualified Data.Map as M
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color3(..), Vertex3(..), Color4(..), ($=))
import qualified Graphics.UI.GLFW as GLFW
import Foreign (sizeOf, nullPtr)

import VoxelHaskell.Block
import VoxelHaskell.GameState
import VoxelHaskell.World

data RenderState = RenderState
  { _vao :: GL.VertexArrayObject
  , _vbo :: GL.BufferObject
  , _vertexAttribute :: GL.AttribLocation
  , _shaderProg :: GL.Program
  }
makeLenses ''RenderState

initRendering :: IO ()
initRendering = void $ GLFW.initialize

makeWindow :: IO ()
makeWindow = do
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"

  GL.polygonMode $= (GL.Fill, GL.Fill)
  --GL.cullFace $= Just GL.Back
  GL.cullFace $= Nothing
  GL.depthFunc $= Just GL.Less

  GLFW.disableSpecial GLFW.MouseCursor
  GLFW.mousePos $= (GL.Position 0 0)

initOGL :: IO RenderState
initOGL = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName

  let vertexAttribute = GL.AttribLocation 0
  GL.bindVertexArrayObject $= Just vao

  vertexShader <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader

  GL.shaderSourceBS vertexShader $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 330 core"
      , "layout (location = 0) in vec3 aPos"
      , "void main(void) {"
      , "  gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);"
      , "}"
      ])

  GL.shaderSourceBS fragmentShader $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 330"
      , "out vec4 fragColor;"
      , "void main(void) {"
      , "  fragColor = vec4(1.0,1.0,1.0,1.0);"
      , "}"
      ])

  GL.compileShader vertexShader
  GL.compileShader fragmentShader

  shaderProg <- GL.createProgram
  GL.attachShader shaderProg vertexShader
  GL.attachShader shaderProg fragmentShader
  GL.attribLocation shaderProg "aPos" $= vertexAttribute
  GL.linkProgram shaderProg
  GL.currentProgram $= Just shaderProg

  pure (RenderState vao vbo vertexAttribute shaderProg)

vertices :: V.Vector Float
vertices = packWorld $ renderWorld initialWorld

renderFrame :: (MonadState (GameState, RenderState) m, MonadIO m) => m ()
renderFrame = do
  (gameState, renderState) <- get

  liftIO $ GL.loadIdentity
  liftIO $ GL.perspective 70 1 0.1 100
  liftIO $ GL.rotate (gameState ^. playerAngle) (Vector3 0 1 (0 :: Float))
  liftIO $ GL.translate (negate <$> gameState ^. playerPos)

  GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.bindVertexArrayObject $= Just (renderState ^. vao)
  GL.bindBuffer GL.ArrayBuffer $= Just (renderState ^. vbo)
  liftIO $ V.unsafeWith vertices $ \v -> GL.bufferData GL.ArrayBuffer $=
    (fromIntegral $ V.length vertices * sizeOf (0 :: Float)
    , v
    , GL.StaticDraw)

  GL.vertexAttribPointer (renderState ^. vertexAttribute) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
  GL.vertexAttribArray (renderState ^. vertexAttribute) $= GL.Enabled

  GL.currentProgram $= Just (renderState ^. shaderProg)

  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral $ V.length vertices)
  liftIO $ GLFW.swapBuffers

toFloat :: Integral n => n -> Float
toFloat = fromIntegral

packWorld :: [Vector3 Float] -> V.Vector Float
packWorld = V.fromList . concatMap (\(Vector3 x y z) -> [x, y, z])

renderWorld :: World -> [Vector3 Float]
renderWorld (World getChunk) =
  --let chunksToRender = [Vector3 x y z | x <- [-1..1], y <- [-1..100], z <- [-1..1]]
  let chunksToRender = [Vector3 0 (-1) 0]
  in flip concatMap chunksToRender $ \pos ->
    (liftA2 (+) (toFloat <$> ((* 16) <$> pos))) <$> renderChunk (getChunk pos)

renderChunk :: Chunk -> [Vector3 Float]
renderChunk (Chunk (M.toList -> blocks)) =
  flip concatMap blocks $ \(pos, block) ->
    (liftA2 (+) (toFloat <$> pos)) <$> renderBlock pos block

renderBlock :: Vector3 Int -> Block -> [Vector3 Float]
renderBlock pos block =
  [ Vector3 (-0.5) (-0.5) 0.5
  , Vector3 0.5 (-0.5) 0.5
  , Vector3 0.5 0.5 0.5
  ]
