module VoxelHaskell.World where

import Control.Lens.TH
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Graphics.Rendering.OpenGL (Vector3(..), Color4(..))

import VoxelHaskell.Block
import VoxelHaskell.WorldGenerator

data Chunk = Chunk
  { _blocks :: Map (Vector3 Int) Block'
  }
makeLenses ''Chunk

data World = World
  { _getChunk :: Vector3 Int -> Chunk
  }
makeLenses ''World

emptyChunk :: Chunk
emptyChunk = Chunk M.empty

fullChunk :: Chunk
fullChunk = Chunk $ M.fromList
  [ (Vector3 x y z, Block (Color4 (fromIntegral x / 15) (fromIntegral y / 15) (fromIntegral z / 15) 1))
  | x <- [0..15]
  , y <- [0..15]
  , z <- [0..15]
  ]

distributeMaybeTuple :: (a, Maybe b) -> Maybe (a, b)
distributeMaybeTuple (a, Nothing) = Nothing
distributeMaybeTuple (a, Just b) = Just (a, b)

generateChunk :: (Vector3 Int -> Block) -> Chunk
generateChunk f = Chunk $ M.fromList $ catMaybes
  [ distributeMaybeTuple (pos, f pos)
  | x <- [0..15]
  , y <- [0..15]
  , z <- [0..15]
  , let pos = Vector3 x y z
  ]

mkWorld :: (Vector3 Int -> Block) -> World
mkWorld generator = World $ \(Vector3 chX chY chZ)
  -> generateChunk $ \(Vector3 localX localY localZ)
                     -> generator (Vector3 (chX * 16 + localX)
                                   (chY * 16 + localY)
                                   (chZ * 16 + localZ))

initialWorld :: World
initialWorld = mkWorld generateWorld
