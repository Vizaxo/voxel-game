module VoxelHaskell.World where

import qualified Data.Map as M
import Data.Map (Map)
import Graphics.Rendering.OpenGL (Vector3(..))

import VoxelHaskell.Block

data Chunk = Chunk
  { _blocks :: Map (Vector3 Int) Block
  }

data World = World
  { _getChunk :: Vector3 Int -> Chunk
  }

emptyChunk :: Chunk
emptyChunk = Chunk M.empty

fullChunk :: Chunk
fullChunk = Chunk $ M.fromList
  [ (Vector3 x y z, (Block black))
  | x <- [0..15]
  , y <- [0..15]
  , z <- [0..15]
  ]

initialWorld :: World
initialWorld = World $ \(Vector3 x y z) ->
  if y >= 0 then emptyChunk else fullChunk
