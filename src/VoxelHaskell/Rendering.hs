module VoxelHaskell.Rendering where

import Control.Applicative
import Control.Monad.State
import Control.Lens
import Data.Distributive (distribute)
import qualified Data.Map as M
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as V
import Linear
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color3(..), Vertex3(..), Color4(..), ($=))
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Foreign (sizeOf, nullPtr, castPtr, plusPtr, with, Ptr)

import VoxelHaskell.Block
import VoxelHaskell.GameState
import VoxelHaskell.World

data RenderState = RenderState
  { _vao :: GL.VertexArrayObject
  , _vbo :: GL.BufferObject
  , _shaderProg :: GL.Program
  , _cachedMesh :: Maybe (V.Vector Float)
  }
makeLenses ''RenderState

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

initOGL :: IO RenderState
initOGL = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName

  GL.bindVertexArrayObject $= Just vao

  vertexShader <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader

  GL.shaderSourceBS vertexShader $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 330 core"
      , "uniform mat4 projection;"
      , "layout (location = 0) in vec3 aPos;"
      , "layout (location = 1) in vec4 colour;"
      , "out vec4 vertexColour;"
      , "void main(void) {"
      , "  gl_Position = projection * vec4(aPos.x, aPos.y, aPos.z, 1.0);"
      , "  vertexColour = colour;"
      , "}"
      ])

  GL.shaderSourceBS fragmentShader $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 330"
      , "out vec4 FragColor;"
      , "in vec4 vertexColour;"
      , "void main(void) {"
      , "  FragColor = vertexColour;"
      , "}"
      ])

  GL.compileShader vertexShader
  GL.compileShader fragmentShader

  shaderProg <- GL.createProgram
  GL.attachShader shaderProg vertexShader
  GL.attachShader shaderProg fragmentShader
  GL.linkProgram shaderProg
  GL.currentProgram $= Just shaderProg

  pure (RenderState vao vbo shaderProg Nothing)

renderFrame :: (MonadState (GameState, RenderState) m, MonadIO m) => m ()
renderFrame = do
  (gameState, renderState) <- get

  GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  vertices <- case (renderState ^. cachedMesh) of
    Just mesh -> pure mesh
    Nothing -> do let vertices = packWorld $ renderWorld gameState
                  modify (set (_2 . cachedMesh) (Just vertices))
                  pure vertices

  GL.bindVertexArrayObject $= Just (renderState ^. vao)
  GL.bindBuffer GL.ArrayBuffer $= Just (renderState ^. vbo)
  liftIO $ V.unsafeWith vertices $ \v -> GL.bufferData GL.ArrayBuffer $=
    (fromIntegral $ V.length vertices * sizeOf (0 :: Float)
    , v
    , GL.StaticDraw)

  let posAttribute = GL.AttribLocation 0
      colourAttribute = GL.AttribLocation 1

  GL.vertexAttribPointer posAttribute $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral (7 * sizeOf (0 :: Float))) nullPtr)
  GL.vertexAttribArray posAttribute $= GL.Enabled

  GL.vertexAttribPointer colourAttribute $=
    (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (fromIntegral (7 * sizeOf (0 :: Float))) (plusPtr nullPtr (3 * sizeOf (0 :: Float))))
  GL.vertexAttribArray colourAttribute $= GL.Enabled

  let (Vector3 posX posY posZ) = gameState ^. playerPos
  let projection = (perspective (90 / 180 * pi) 0.5 0.1 50 :: M44 GL.GLfloat)
        !*! mkTransformation
        (axisAngle (V3 0 1 0) (gameState ^. playerAngle / 180 * pi))
        (V3 0 0 0)
        !*! mkTransformation
        (axisAngle (V3 0 1 0) 0)
        (V3 (-posX) (-posY) (-posZ))
  GL.currentProgram $= Just (renderState ^. shaderProg)

  GL.UniformLocation projectionLocation <- GL.get (GL.uniformLocation (renderState ^. shaderProg) "projection")
  liftIO $ with (distribute $ projection) $ \ptr ->
    GL.glUniformMatrix4fv projectionLocation 1 0 (castPtr ptr)

  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral $ V.length vertices)
  liftIO $ GLFW.swapBuffers

toFloat :: Integral n => n -> Float
toFloat = fromIntegral

packWorld :: [(Vector3 Float, Color4 Float)] -> V.Vector Float
packWorld = V.fromList
  . concatMap (\(Vector3 x y z, Color4 r g b a) -> [x, y, z, r, g, b, a])

renderWorld :: GameState -> [(Vector3 Float, Color4 Float)]
renderWorld state =
  let chunksToRender = [Vector3 x y z | x <- [-1..1], y <- [-1..100], z <- [-1..1]]
  in flip concatMap chunksToRender $ \pos ->
    over (mapped . _1) (liftA2 (+) (toFloat <$> ((* 16) <$> pos))) (renderChunk ((state ^. world . getChunk) pos))

renderChunk :: Chunk -> [(Vector3 Float, Color4 Float)]
renderChunk (Chunk (M.toList -> blocks)) =
  flip concatMap blocks $ \(pos, block) ->
    over (mapped . _1) (liftA2 (+) (toFloat <$> pos)) (renderBlock pos block)

renderBlock :: Vector3 Int -> Block -> [(Vector3 Float, Color4 Float)]
renderBlock pos (Block colour) =
  (,colour) <$>
  -- front
  [ Vector3 (-0.5) (-0.5) 0.5
  , Vector3 0.5 (-0.5) 0.5
  , Vector3 0.5 0.5 0.5
  , Vector3 0.5 0.5 0.5
  , Vector3 (-0.5) 0.5 0.5
  , Vector3 (-0.5) (-0.5) 0.5
  -- right
  , Vector3 0.5 (-0.5) (-0.5)
  , Vector3 0.5 0.5 (-0.5)
  , Vector3 0.5 0.5 0.5
  , Vector3 0.5 0.5 0.5
  , Vector3 0.5 (-0.5) 0.5
  , Vector3 0.5 (-0.5) (-0.5)
  -- top
  , Vector3 0.5 0.5 0.5
  , Vector3 0.5 0.5 (-0.5)
  , Vector3 (-0.5) 0.5 (-0.5)
  , Vector3 (-0.5) 0.5 (-0.5)
  , Vector3 (-0.5) 0.5 0.5
  , Vector3 0.5 0.5 0.5
  -- back
  , Vector3 (-0.5) (-0.5) (-0.5)
  , Vector3 (-0.5) 0.5 (-0.5)
  , Vector3 0.5 0.5 (-0.5)
  , Vector3 0.5 0.5 (-0.5)
  , Vector3 0.5 (-0.5) (-0.5)
  , Vector3 (-0.5) (-0.5) (-0.5)
  -- left
  , Vector3 (-0.5) (-0.5) (-0.5)
  , Vector3 (-0.5) (-0.5) 0.5
  , Vector3 (-0.5) 0.5 0.5
  , Vector3 (-0.5) 0.5 0.5
  , Vector3 (-0.5) 0.5 (-0.5)
  , Vector3 (-0.5) (-0.5) (-0.5)
  -- bottom
  , Vector3 0.5 (-0.5) 0.5
  , Vector3 (-0.5) (-0.5) 0.5
  , Vector3 (-0.5) (-0.5) (-0.5)
  , Vector3 (-0.5) (-0.5) (-0.5)
  , Vector3 0.5 (-0.5) (-0.5)
  , Vector3 0.5 (-0.5) 0.5
  ]
