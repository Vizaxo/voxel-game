module VoxelHaskell.Rendering.Window where

import Control.Applicative
import Control.Monad.State
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (Vector3(..), Color3(..), Vertex3(..), Color4(..), ($=))
import qualified Graphics.UI.GLFW as GLFW

import VoxelHaskell.Game

makeWindow :: IO ()
makeWindow = do
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"

  GLFW.disableSpecial GLFW.MouseCursor
  GLFW.mousePos $= (GL.Position 0 0)

  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Nothing

  GL.depthFunc $= Just GL.Less
  GL.loadIdentity

black :: Color3 Float
black = Color3 0 0 0

basePlane :: Int -> [Block]
basePlane size =
  [ Block x y z black
  | x <- [-size..size]
  , y <- [0]
  , z <- [-size..size]
  ]

blocks :: [Block]
blocks = basePlane 5
  <> [Block 0 1 0 black]

initialGameState :: GameState
initialGameState = GameState
  { _playerPos = Vector3 0 3 10
  , _playerAngle = 0
  }

game :: IO ()
game = void $ runStateT (forever mainLoop) initialGameState

mainLoop :: (MonadState GameState m, MonadIO m) => m ()
mainLoop = do
  state <- get
  liftIO $ GL.loadIdentity
  liftIO $ GL.perspective 70 1 0.1 100
  liftIO $ GL.rotate (state ^. playerAngle) (Vector3 0 1 (0 :: Float))
  liftIO $ GL.translate (negate <$> state ^. playerPos)
  --liftIO $ GL.rotate 30 (Vector3 1 0 (0 :: Float))

  liftIO $ GL.clearColor $= Color4 0 0 0 0
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  handleInput
  mapM_ (liftIO . renderBlock) blocks

  liftIO GLFW.swapBuffers

handleInput :: (MonadState GameState m, MonadIO m) => m ()
handleInput = do
  onPress (GLFW.CharKey ',')
    $ modify (over playerPos (liftA2 (+) (Vector3 0 0 (-0.1))))
  onPress (GLFW.CharKey 'O')
    $ modify (over playerPos (liftA2 (+) (Vector3 0 0 0.1)))
  onPress (GLFW.CharKey 'A')
    $ modify (over playerPos (liftA2 (+) (Vector3 (-0.1) 0 0)))
  onPress (GLFW.CharKey 'E')
    $ modify (over playerPos (liftA2 (+) (Vector3 (0.1) 0 0)))

  GL.Position posX posY <- liftIO (GL.get GLFW.mousePos)
  modify (over playerAngle (+ (fromIntegral posX / 7)))
  GLFW.mousePos $= (GL.Position 0 0)

onPress :: MonadIO m => GLFW.Key -> m () -> m ()
onPress k ma =
  liftIO (GLFW.getKey k) >>= \case
    GLFW.Press -> ma
    GLFW.Release -> pure ()


data Block = Block
  { x :: Int
  , y :: Int
  , z :: Int
  , colour :: Color3 Float
  }

toFloat :: Integral n => n -> Float
toFloat = fromIntegral

renderBlock :: Block -> IO ()
renderBlock (Block (toFloat -> x) (toFloat -> y) (toFloat -> z) rgb) = do
  GL.renderPrimitive GL.Quads $ do
    let vertex3f x y z = GL.vertex $ Vertex3 x y z
      -- front
    GL.preservingMatrix $ do
      GL.color (Color3 (0 :: Float) 1 0)
      vertex3f (x - 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)

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
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z - 0.5)

      --bottom
      GL.color (Color3 (0.5 :: Float) 0.5 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y - 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y - 0.5) (z - 0.5)

      --top
      GL.color (Color3 (1 :: Float) 0.5 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z + 0.5)
      vertex3f (x - 0.5) (y + 0.5) (z - 0.5)
      vertex3f (x + 0.5) (y + 0.5) (z - 0.5)
