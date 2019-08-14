module VoxelHaskell.Rendering where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.MultiState
import Control.Lens
import Data.Distributive (distribute)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as V
import Linear hiding (angle)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color4(..), ($=))
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Foreign (sizeOf, nullPtr, castPtr, plusPtr, with)

import VoxelHaskell.Block
import VoxelHaskell.Player
import VoxelHaskell.Utils
import VoxelHaskell.World

data MeshCache = MeshCache
  { _mesh :: Maybe (V.Vector Float)
  , _renderedChunks :: S.Set (Vector3 Int)
  , _dirty :: Bool
  }
makeLenses ''MeshCache

emptyCache :: MeshCache
emptyCache = MeshCache Nothing S.empty False

data RenderState = RenderState
  { _vao :: GL.VertexArrayObject
  , _vbo :: GL.BufferObject
  , _shaderProg :: GL.Program
  , _cachedMesh :: MeshCache
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

  pure (RenderState vao vbo shaderProg emptyCache)

generateMesh
  :: (MonadMultiGet Player m, MonadMultiGet World m
    , MonadMultiState RenderState m, MonadIO m) => m (V.Vector Float)
generateMesh = do
  renderState <- mGet

  let cachedMesh' = renderState ^. cachedMesh
  toRender <- chunksToRender
  case (cachedMesh' ^. mesh
       , cachedMesh' ^. dirty
       , cachedMesh' ^. renderedChunks == toRender) of
    (Just mesh, False, True) -> pure mesh
    _ -> do vertices <- packWorld <$> renderWorld
            mModify (set (cachedMesh . mesh) (Just vertices))
            mModify (set (cachedMesh . dirty) False)
            mModify (set (cachedMesh . renderedChunks) toRender)
            pure vertices

chunksToRender :: MonadMultiGet Player m => m (S.Set (Vector3 Int))
chunksToRender = do
  player <- mGet
  let (Vector3 (toChunkPos -> posX) (toChunkPos -> posY) (toChunkPos -> posZ))
        = player ^. pos
  pure $ S.fromList [Vector3 x y z | x <- [posX - 1..posX + 1], y <- [posY - 1..posY + 1], z <- [posZ - 1..posZ + 1]]

renderFrame
  :: (MonadMultiGet Player m, MonadMultiGet World m, MonadMultiState RenderState m
    , MonadIO m) => m ()
renderFrame = do
  player <- mGet
  renderState <- mGet

  GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  vertices <- generateMesh

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

  let (Vector3 posX posY posZ) = player ^. pos
  let projection = (perspective (90 / 180 * pi) 0.5 0.1 50 :: M44 GL.GLfloat)
        !*! mkTransformation
        (axisAngle (V3 0 1 0) (player ^. angle / 180 * pi))
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

toChunkPos :: Float -> Int
toChunkPos x = round x `div` 16

renderWorld :: (MonadMultiGet Player m, MonadMultiGet World m) => m [(Vector3 Float, Color4 Float)]
renderWorld = do
  player <- mGet

  let (Vector3 (toChunkPos -> posX) (toChunkPos -> posY) (toChunkPos -> posZ))
        = player ^. pos
  world <- mGet
  chunks <- S.toList <$> chunksToRender
  pure $ flip concatMap chunks
    $ \pos ->
        over (mapped . _1) (liftA2 (+) (toFloat <$> ((* 16) <$> pos)))
        (renderChunk ((world ^. getChunk) pos))

renderChunk :: Chunk -> [(Vector3 Float, Color4 Float)]
renderChunk (Chunk (M.toList -> blocks)) =
  flip concatMap blocks $ \(pos, block) ->
    over (mapped . _1) (liftA2 (+) (toFloat <$> pos)) (renderBlock pos block)

renderBlock :: Vector3 Int -> Block' -> [(Vector3 Float, Color4 Float)]
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
