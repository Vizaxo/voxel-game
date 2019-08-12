module Main where

import VoxelHaskell.Rendering.Initialisation
import VoxelHaskell.Rendering.Window

main = do
  initRendering
  makeWindow
  game
