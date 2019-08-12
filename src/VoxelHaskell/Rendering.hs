module VoxelHaskell.Rendering where

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

renderFrame :: (MonadState (GameState, RenderState) m, MonadIO m) => m ()
renderFrame = do
  (gameState, renderState) <- get
  --liftIO $ print (gameState ^. playerPos, gameState ^. playerAngle)

  let vertices = V.fromList [ -0.5, -0.5, 0
                            , 0.5, -0.5, 0
                            , 0, 0.5, 0
                            ] :: V.Vector Float

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

  liftIO $ GL.drawArrays GL.Triangles 0 3
  liftIO $ GLFW.swapBuffers

toFloat :: Integral n => n -> Float
toFloat = fromIntegral

renderWorld :: MonadIO m => World -> m ()
renderWorld (World getChunk) =
  let chunksToRender = [Vector3 x y z | x <- [-1..1], y <- [-1..100], z <- [-1..1]]

  in flip mapM_ chunksToRender $ \pos ->
  liftIO $ GL.preservingMatrix $ do
    GL.translate ((toFloat . (*16)) <$> pos)
    renderChunk (getChunk pos)

renderChunk :: MonadIO m => Chunk -> m ()
renderChunk (Chunk (M.toList -> blocks)) = flip mapM_ blocks $ \(pos, block) ->
  liftIO $ renderBlock pos block

renderBlock :: Vector3 Int -> Block -> IO ()
renderBlock (Vector3 (toFloat -> x) (toFloat -> y) (toFloat -> z)) (Block rgb) = do
  GL.renderPrimitive GL.Quads $ do
    let vertex3f x y z = GL.vertex $ Vertex3 x y z
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
