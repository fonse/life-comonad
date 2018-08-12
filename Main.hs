module Main where

import Grid
import Life

viewportWidth = 4
viewportHeight = 4
generationCount = 7

main :: IO ()
main = mapM_ putStr . map (showGrid viewportHeight viewportWidth) . take generationCount $ (life initialGrid)

initialGrid :: Grid Cell
initialGrid = Store f (0,0) where
  f (0,1) = On
  f (1,2) = On
  f (2,0) = On
  f (2,1) = On
  f (2,2) = On
  f _     = Off
