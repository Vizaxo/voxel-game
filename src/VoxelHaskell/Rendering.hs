module VoxelHaskell.Rendering where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.MultiState
import Control.Lens
import Data.Distributive (distribute)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Storable as V
import Linear hiding (angle)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color4(..), ($=))
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Foreign (sizeOf, nullPtr, castPtr, plusPtr, with)

import VoxelHaskell.Block
import VoxelHaskell.Camera
import VoxelHaskell.Player
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

viewDistance :: Int
viewDistance = 1

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

  pure (RenderState vao vbo shaderProg emptyCache)

generateMesh
  :: (MonadMultiGet Player m, MonadMultiGet World m
    , MonadState RenderState m) => m (V.Vector Float)
generateMesh = do
  renderState <- get

  let cachedMesh' = renderState ^. cachedMesh
  toRender <- chunksToRender
  case (cachedMesh' ^. mesh
       , cachedMesh' ^. dirty
       , cachedMesh' ^. renderedChunks == toRender) of
    (Just mesh, False, True) -> pure mesh
    _ -> do vertices <- packWorld <$> renderWorld
            let cache' = MeshCache (Just vertices) toRender False

            -- Perform the calculations before the STM transaction starts
            seq vertices (pure ())

            modify (set cachedMesh cache')
            pure vertices

getVertices
  :: (MonadMultiGet Player m, MonadState RenderState m) => m (V.Vector Float)
getVertices = do
  renderState <- get
  case (renderState ^. cachedMesh . mesh) of
    Just mesh -> do
      pure mesh
    _ -> pure (V.empty)

chunksToRender :: MonadMultiGet Player m => m (S.Set (Vector3 Int))
chunksToRender = do
  player <- mGet
  let (V3 (toChunkPos -> posX) (toChunkPos -> posY) (toChunkPos -> posZ))
        = player ^. pos
  pure $ S.fromList [Vector3 x y z | x <- [posX - viewDistance..posX + viewDistance], y <- [posY - viewDistance..posY + viewDistance], z <- [posZ - viewDistance..posZ + viewDistance]]

renderFrame
  :: (MonadMultiGet Player m, MonadState RenderState m
    , MonadIO m) => m ()
renderFrame = do
  renderState <- get

  GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  vertices <- getVertices

  GL.bindVertexArrayObject $= Just (renderState ^. vao)
  GL.bindBuffer GL.ArrayBuffer $= Just (renderState ^. vbo)
  liftIO $ V.unsafeWith vertices $ \v -> GL.bufferData GL.ArrayBuffer $=
    (fromIntegral $ V.length vertices * sizeOf (0 :: Float)
    , v
    , GL.DynamicDraw)

  let posAttribute = GL.AttribLocation 0
      colourAttribute = GL.AttribLocation 1

  GL.vertexAttribPointer posAttribute $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral (7 * sizeOf (0 :: Float))) nullPtr)
  GL.vertexAttribArray posAttribute $= GL.Enabled

  GL.vertexAttribPointer colourAttribute $=
    (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (fromIntegral (7 * sizeOf (0 :: Float))) (plusPtr nullPtr (3 * sizeOf (0 :: Float))))
  GL.vertexAttribArray colourAttribute $= GL.Enabled

  cam <- cameraMatrix
  GL.currentProgram $= Just (renderState ^. shaderProg)

  GL.UniformLocation projectionLocation <- GL.get (GL.uniformLocation (renderState ^. shaderProg) "projection")
  liftIO $ with (distribute $ cam) $ \ptr ->
    GL.glUniformMatrix4fv projectionLocation 1 0 (castPtr ptr)

  liftIO $ GL.drawArrays GL.Points 0 (fromIntegral $ V.length vertices)
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
  world <- mGet
  chunks <- S.toList <$> chunksToRender
  pure $ flip concatMap chunks
    $ \pos ->
        over (mapped . _1) (liftA2 (+) (toFloat <$> ((* 16) <$> pos)))
        (renderChunk ((world ^. getChunk) pos))

renderChunk :: Chunk -> [(Vector3 Float, Color4 Float)]
renderChunk (Chunk blocks) = uncurry renderBlock <$> M.toList blocks

renderBlock :: Vector3 Int -> Block' -> (Vector3 Float, Color4 Float)
renderBlock pos (Block colour) = (toFloat <$> pos, colour)
