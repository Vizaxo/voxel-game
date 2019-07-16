module VoxelHaskell.Rendering.Initialisation where

import Control.Monad
import Graphics.UI.GLFW as GLFW

initRendering :: IO ()
initRendering = void $ GLFW.initialize
