{-# LANGUAGE TypeFamilies #-}

module Grid
    ( Grid(..)
    , showGrid
    ) where

import Comonad
import Representable
import Control.Monad

{- |
An infinite square grid focused on a specific point.

'Representable' by @(Int,Int)@. Namely, instances of 'Grid' can be converted from and to functions @(Int,Int) -> a@.

This mapping is done considering the grid is focused on @(0,0)@ and that the positive directions are down and right.

For example:

@
gliderGrid = tabulate f where
  f (1,2) = 1
  f (2,3) = 1
  f (3,1) = 1
  f (3,2) = 1
  f (3,3) = 1
  f _     = 0
@
-}
data Grid a = Grid { nw :: Grid a , n :: Grid a , ne :: Grid a
                   , w  :: Grid a , val :: a    , e  :: Grid a
                   , sw :: Grid a , s :: Grid a , se :: Grid a }

instance Functor Grid where
  fmap f grid = Grid (fmap f (nw grid)) (fmap f (n grid)) (fmap f (ne grid))
                     (fmap f ( w grid)) (f (val grid))    (fmap f ( e grid))
                     (fmap f (sw grid)) (fmap f (s grid)) (fmap f (se grid))

instance Comonad Grid where
  extract = val
  duplicate grid = Grid (duplicate (nw grid)) (duplicate (n grid)) (duplicate (ne grid))
                        (duplicate ( w grid)) grid                 (duplicate ( e grid))
                        (duplicate (sw grid)) (duplicate (s grid)) (duplicate (se grid))

instance Representable Grid where
  type Key Grid = (Int,Int)

  index grid (0,0) = extract grid
  index grid (0,y) = if y < 0 then index (w grid) (0,y+1) else index (e grid) (0,y-1)
  index grid (x,y) = if x < 0 then index (n grid) (x+1,y) else index (s grid) (x-1,y)

  tabulate f = tabulate' f (0,0)
    where tabulate' f (x,y) = Grid (tabulate' f (x-1,y-1)) (tabulate' f (x-1,y)) (tabulate' f (x-1,y+1))
                                   (tabulate' f (x,y-1))   (f (x,y))             (tabulate' f (x,y+1))
                                   (tabulate' f (x+1,y-1)) (tabulate' f (x+1,y)) (tabulate' f (x+1,y+1))

-- | Show a section of the grid, spanning the given height and width with the focused cell as the upper-left corner.
showGrid :: Show a => Int -> Int -> Grid a -> String
showGrid height width grid = let index' = index grid
                                 cells = [ [ index' (i,j) | j <- [0..height] ] | i <- [0..width] ]
                                 showRow as = join (map show as) ++ "\n"
                             in join (map showRow cells)
