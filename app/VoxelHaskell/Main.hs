module Main where

import Control.Monad
import VoxelHaskell.Rendering.Initialisation
import VoxelHaskell.Rendering.Window

main = do
  initRendering
  makeWindow
  forever mainLoop
