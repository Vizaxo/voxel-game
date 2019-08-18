module VoxelHaskell.Rendering.Types where

import Control.Lens
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color4(..))
import Foreign (Storable, sizeOf)

data RenderState = RenderState
  { _vao :: GL.VertexArrayObject
  , _vbo :: GL.BufferObject
  , _shaderProg :: GL.Program
  }
makeLenses ''RenderState

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
  { _worldMesh :: Maybe ByteString
  , _chunkVertices :: M.Map (Vector3 Int) ByteString
  , _renderedChunks :: S.Set (Vector3 Int)
  , _dirty :: Bool
  }
makeLenses ''MeshCache

emptyMeshCache :: MeshCache
emptyMeshCache = MeshCache Nothing M.empty S.empty False

