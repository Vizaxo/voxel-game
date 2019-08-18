module VoxelHaskell.Rendering where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder
import Data.Distributive (distribute)
import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Word
import Linear hiding (angle)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color4(..), ($=))
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Foreign (Storable, sizeOf, nullPtr, castPtr, plusPtr, with)

import VoxelHaskell.Block
import VoxelHaskell.Camera
import VoxelHaskell.Player
import VoxelHaskell.STMState
import VoxelHaskell.Utils
import VoxelHaskell.World

newtype FaceBitmask = FaceBitmask Word32
  deriving (Eq, Show, Bits, Storable)

allFaces, noFaces, posZ, negZ, posX, negX, top, bottom :: FaceBitmask
allFaces = top .|. bottom .|. posX .|. negX .|. posZ .|. negZ
noFaces  = FaceBitmask 0x00;
posZ     = FaceBitmask 0x01;
negZ     = FaceBitmask 0x02;
negX     = FaceBitmask 0x04;
posX     = FaceBitmask 0x08;
top      = FaceBitmask 0x10;
bottom   = FaceBitmask 0x20;

data Vertex = Vertex
  { _vertPos :: Vector3 Int
  , _colour :: Color4 Float
  , _faces :: FaceBitmask
  }
  deriving Show
makeLenses ''Vertex

vertexSize :: Int
vertexSize = sizeOf (undefined :: Vector3 Float) + sizeOf (undefined :: Color4 Float)
  + sizeOf (undefined :: FaceBitmask)

data MeshCache = MeshCache
  { _mesh :: Maybe ByteString
  , _chunkVertices :: M.Map (Vector3 Int) ByteString
  , _renderedChunks :: S.Set (Vector3 Int)
  , _dirty :: Bool
  }
makeLenses ''MeshCache

emptyCache :: MeshCache
emptyCache = MeshCache Nothing M.empty S.empty False

data RenderState = RenderState
  { _vao :: GL.VertexArrayObject
  , _vbo :: GL.BufferObject
  , _shaderProg :: GL.Program
  , _cachedMesh :: MeshCache
  }
makeLenses ''RenderState

viewDistance :: Int
viewDistance = 10

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
  :: (MonadGet Player m, MonadGet World m
    , MonadState RenderState m, MonadIO m) => m ByteString
generateMesh = do
  renderState <- get

  let cachedMesh' = renderState ^. cachedMesh
  toRender <- chunksToRender
  case (cachedMesh' ^. mesh
       , cachedMesh' ^. dirty
       , cachedMesh' ^. renderedChunks == S.fromList toRender) of
    (Just mesh, False, True) -> pure mesh
    _ -> do --Generate the mesh based on pre-generated chunks
            let chunks = catMaybes $ flip M.lookup
                         (renderState ^. cachedMesh . chunkVertices)
                         <$> toRender
            let vertices = BS.concat chunks

            -- Perform the calculations before the STM transaction starts
            seq vertices (pure ())

            modify (over cachedMesh (set renderedChunks (S.fromList toRender)
                                     . set dirty False
                                     . set mesh (Just vertices)))
            liftIO $ print $ "Generated " <> show (BS.length vertices `div` sizeOf (0 :: Float)) <> " vertices"
            pure vertices

getVertices
  :: (MonadGet Player m, MonadState RenderState m) => m ByteString
getVertices = do
  renderState <- get
  case (renderState ^. cachedMesh . mesh) of
    Just mesh -> do
      pure mesh
    _ -> pure BS.empty

chunksToRender :: MonadGet Player m => m [Vector3 Int]
chunksToRender = do
  player <- get
  let chPos = toChunkPos <$> (player ^. pos)
      distanceToPlayer (vector3ToV3 -> v) = magnitudeSquared $ v - chPos
  pure $ sortOn distanceToPlayer
    $ [Vector3 x y z
      | x <- [chPos^._x - viewDistance..chPos^._x + viewDistance]
      , y <- [chPos^._y - viewDistance..chPos^._y + viewDistance]
      , z <- [chPos^._z - viewDistance..chPos^._z + viewDistance]]

renderFrame
  :: (MonadGet Player m, MonadState RenderState m
    , MonadIO m) => m ()
renderFrame = do
  renderState <- get

  GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  vertices <- getVertices
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

toFloat :: Integral n => n -> Float
toFloat = fromIntegral

packWorld :: [Vertex] -> ByteString
packWorld = LBS.toStrict . toLazyByteString .
  foldMap (\(Vertex (Vector3 x y z) (Color4 r g b a) (FaceBitmask faces)) ->
             floatLE (fromIntegral x) <> floatLE (fromIntegral y) <> floatLE (fromIntegral z) <>
             floatLE r <> floatLE g <> floatLE b <> floatLE a <>
             word32LE faces)

toChunkPos :: Float -> Int
toChunkPos x = round x `div` 16

faceMapping :: [(Vector3 Int, FaceBitmask)]
faceMapping =
  [ (Vector3 0 1 0, top)
  , (Vector3 0 (-1) 0, bottom)
  , (Vector3 1 0 0, posX)
  , (Vector3 (-1) 0 0, negX)
  , (Vector3 0 0 1, posZ)
  , (Vector3 0 0 (-1), negZ)
  ]

renderWorld :: (MonadGet Player m, MonadGet World m) => m [Vertex]
renderWorld = do
  world <- get
  chunks <- chunksToRender
  let renderChunkAtPos pos =
        (renderChunk world pos ((world ^. getChunk) pos))
  pure $ concatMap renderChunkAtPos chunks

-- | When run continually in a separate thread, it will populate the
-- chunk mesh cache with the chunks nearest the player
generateChunks
  :: (MonadGet World m, MonadState RenderState m, MonadGet Player m, MonadIO m)
  => m ()
generateChunks = mapMUntil generateChunkMesh =<< chunksToRender
  where
    -- Restart generation each time a chunk is successfully generated
    -- so when the player moves the next chunks generated is near them
    mapMUntil mf [] = pure ()
    mapMUntil mf (x:xs) = do
      res <- mf x
      unless res (mapMUntil mf xs)

generateChunkMesh
  :: (MonadGet World m , MonadState RenderState m, MonadIO m)
  => Vector3 Int -> m Bool
generateChunkMesh pos = do
  renderState <- get
  case M.lookup pos (renderState ^. cachedMesh . chunkVertices) of
    Just vs -> pure False --TODO: when chunks can be edited, need to check if dirty
    Nothing -> do
      liftIO $ putStrLn $ "Generating chunk mesh at pos " <> show pos
      world <- get
      let chunk = renderChunk world pos ((world ^. getChunk) pos)
      let vertices = packWorld chunk
      seq vertices (pure ())
      modify (over cachedMesh
              (set dirty True
                . over chunkVertices (M.insert pos vertices)))
      pure True

renderChunk :: World -> Vector3 Int -> Chunk -> [Vertex]
renderChunk world pos (Chunk blocks) =
  cullEmptyCubes
  $ fmap (cullAdjacentFaces . over vertPos (liftA2 (+) ((* 16) <$> pos)))
  $ uncurry renderBlock <$> M.toList blocks
  where
    cullAdjacentFaces vert =
      flip (set faces) vert $ foldr (.|.) noFaces $ flip fmap faceMapping $
        \(dir, face) ->
          getBlock' world (liftA2 (+) (vert ^. vertPos) dir) & \case
            Nothing -> face
            Just _ -> noFaces
    cullEmptyCubes = filter (\(Vertex _ _ (FaceBitmask n)) -> n /= 0)


renderBlock :: Vector3 Int -> Block' -> Vertex
renderBlock pos (Block colour) = Vertex pos colour allFaces
