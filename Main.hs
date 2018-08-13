module Main where

import Grid
import Life

viewportWidth = 50
viewportHeight = 20

main :: IO ()
main = mapM_ putStr . map (showGrid viewportHeight viewportWidth) $ (life initialGrid)

initialGrid :: Grid Cell
initialGrid = Store f (0,0) where
  f (5,1) = On
  f (5,2) = On
  f (6,1) = On
  f (6,2) = On
  f (5,11) = On
  f (6,11) = On
  f (7,11) = On
  f (4,12) = On
  f (3,13) = On
  f (3,14) = On
  f (8,12) = On
  f (9,13) = On
  f (9,14) = On
  f (6,15) = On
  f (4,16) = On
  f (5,17) = On
  f (6,17) = On
  f (7,17) = On
  f (6,18) = On
  f (8,16) = On
  f (3,21) = On
  f (4,21) = On
  f (5,21) = On
  f (3,22) = On
  f (4,22) = On
  f (5,22) = On
  f (2,23) = On
  f (6,23) = On
  f (1,25) = On
  f (2,25) = On
  f (6,25) = On
  f (7,25) = On
  f (3,35) = On
  f (4,35) = On
  f (3,36) = On
  f (4,36) = On
  f _     = Off
