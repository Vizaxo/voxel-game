module VoxelHaskell.World where

import Control.Lens
import Control.Monad.Trans.MultiState
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.MemoTrie
import Graphics.Rendering.OpenGL (Vector3(..), Color4(..))
import Linear
import GHC.Generics

import VoxelHaskell.Block
import VoxelHaskell.Utils

deriving instance Generic a => Generic (Vector3 a)
deriving instance Generic Int

instance (HasTrie a, Generic a) => HasTrie (Vector3 a) where
  newtype (Vector3 a :->: b) = Vector3Tree { unVector3Tree :: Reg (Vector3 a) :->: b }
  trie = trieGeneric Vector3Tree
  untrie = untrieGeneric unVector3Tree
  enumerate = enumerateGeneric unVector3Tree

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
mkWorld generator = World $ memo $ \(Vector3 chX chY chZ)
  -> generateChunk $ \(Vector3 localX localY localZ)
                            -> generator (Vector3 (chX * 16 + localX)
                                          (chY * 16 + localY)
                                          (chZ * 16 + localZ))

worldToChunkPos :: Vector3 Int -> Vector3 Int
worldToChunkPos pos = (`div` 16) <$> pos

toPosInChunk :: Vector3 Int -> Vector3 Int
toPosInChunk pos = (`mod` 16) <$> pos

getBlock :: MonadMultiGet World m => V3 Int -> m Block
getBlock (v3ToVector3 -> pos) = do
  world <- mGet
  let chunk = (world ^. getChunk) (worldToChunkPos pos)
  pure (M.lookup (toPosInChunk pos) (chunk ^. blocks))

getBlock' :: World -> Vector3 Int -> Block
getBlock' world pos =
  let chunk = (world ^. getChunk) (worldToChunkPos pos)
  in M.lookup (toPosInChunk pos) (chunk ^. blocks)
