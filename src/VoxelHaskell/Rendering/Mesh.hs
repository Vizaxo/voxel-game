module VoxelHaskell.Rendering.Mesh where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder
import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.Set as S
import Linear hiding (angle)
import Graphics.Rendering.OpenGL (Vector3(..), Color4(..))
import Foreign (sizeOf)

import VoxelHaskell.Block
import VoxelHaskell.Player
import VoxelHaskell.Rendering.Types
import VoxelHaskell.STMState
import VoxelHaskell.Utils
import VoxelHaskell.World

viewDistance :: Int
viewDistance = 10

toChunkPos :: Float -> Int
toChunkPos x = round x `div` 16

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

generateWorldMesh
  :: (MonadGet Player m, MonadGet World m
    , MonadState MeshCache m, MonadIO m) => m ByteString
generateWorldMesh = do
  meshCache <- get
  toRender <- chunksToRender
  case (meshCache ^. worldMesh
       , meshCache ^. dirty
       , meshCache ^. renderedChunks == S.fromList toRender) of
    (Just worldMesh, False, True) -> pure worldMesh
    _ -> do --Generate the world mesh based on chunks vertex cache
      let chunks = catMaybes $ flip M.lookup
                   (meshCache ^. chunkVertices)
                   <$> toRender
          vertices = BS.concat chunks

      -- Perform the calculations before the STM transaction starts
      seq vertices (pure ())

      modify (set renderedChunks (S.fromList toRender)
               . set dirty False
               . set worldMesh (Just vertices))
      liftIO $ print $ "Generated "
        <> show (BS.length vertices `div` sizeOf (0 :: Float))
        <> " vertices"
      pure vertices

getWorldMesh :: MonadGet MeshCache m => m ByteString
getWorldMesh = fromMaybe BS.empty . view worldMesh <$> get

-- | When run continually in a separate thread, it will populate the
-- chunk mesh cache with the chunks nearest the player
generateChunkVertices
  :: (MonadGet World m, MonadState MeshCache m, MonadGet Player m, MonadIO m)
  => m ()
generateChunkVertices = mapMUntil generateChunkVerticesAt =<< chunksToRender
  where
    -- Restart generation each time a chunk is successfully generated so
    -- that when the player moves the next chunks generated are near them
    mapMUntil mf [] = pure ()
    mapMUntil mf (x:xs) = do
      res <- mf x
      unless res (mapMUntil mf xs)

-- Returns whether a new chunk's mesh was inserted into the cache
generateChunkVerticesAt
  :: (MonadGet World m , MonadState MeshCache m, MonadIO m)
  => Vector3 Int -> m Bool
generateChunkVerticesAt pos = do
  meshCache <- get
  case M.lookup pos (meshCache ^. chunkVertices) of
    Just vs -> pure False --TODO: when chunks can be edited, need to check if dirty
    Nothing -> do
      liftIO $ putStrLn $ "Generating chunk mesh at pos " <> show pos
      world <- get
      let chunk = renderChunk world pos ((world ^. getChunk) pos)
      let vertices = packVertices chunk
      seq vertices (pure ())
      modify (set dirty True . over chunkVertices (M.insert pos vertices))
      pure True

packVertices :: [Vertex] -> ByteString
packVertices = LBS.toStrict . toLazyByteString .
  foldMap (\(Vertex (Vector3 x y z) (Color4 r g b a) (FaceBitmask faces)) ->
             floatLE (fromIntegral x)
             <> floatLE (fromIntegral y)
             <> floatLE (fromIntegral z)
             <> floatLE r <> floatLE g <> floatLE b <> floatLE a
             <> word32LE faces)

faceDirections :: [(Vector3 Int, FaceBitmask)]
faceDirections =
  [ (Vector3 0 1 0, top)
  , (Vector3 0 (-1) 0, bottom)
  , (Vector3 1 0 0, posX)
  , (Vector3 (-1) 0 0, negX)
  , (Vector3 0 0 1, posZ)
  , (Vector3 0 0 (-1), negZ)
  ]

renderChunk :: World -> Vector3 Int -> Chunk -> [Vertex]
renderChunk world pos (Chunk blocks) =
  cullEmptyCubes
  $ fmap (cullAdjacentFaces . over vertPos (liftA2 (+) ((* 16) <$> pos)))
  $ uncurry renderBlock <$> M.toList blocks
  where
    cullAdjacentFaces vert =
      flip (set faces) vert $ foldr (.|.) noFaces $ flip fmap faceDirections $
        \(dir, face) ->
          getBlock' world (liftA2 (+) (vert ^. vertPos) dir) & \case
            Nothing -> face
            Just _ -> noFaces
    cullEmptyCubes = filter (\(Vertex _ _ (FaceBitmask n)) -> n /= 0)

renderBlock :: Vector3 Int -> Block' -> Vertex
renderBlock pos (Block colour) = Vertex pos colour allFaces
