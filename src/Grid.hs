module Grid
    ( Grid(..)
    , Store(..)
    , showGrid
    , shiftGrid
    ) where

import Comonad

-- | Generic comonad dual to 'State' monad.
-- Essentially a representable functor along with an instance of its index type.
data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap g (Store f s) = Store (g . f) s

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

{- |
An infinite square grid focused on a specific cell.

Represented by a function @(Int,Int) -> a@ which indicates the value of the grid for all points, along with a pair of coordinates to use as the focused cell.

For example:

@
gliderGrid = Store f (0,0) where
  f (1,2) = 1
  f (2,3) = 1
  f (3,1) = 1
  f (3,2) = 1
  f (3,3) = 1
  f _     = 0
@
-}
type Grid a = Store (Int,Int) a

-- | Show a section of the grid, spanning the given height and width with the focused cell as the upper-left corner.
showGrid :: Show a => Int -> Int -> Grid a -> String
showGrid height width (Store f (x,y)) = let cells = [ [ f (i,j) | j <- [x..x+height] ] | i <- [y..y+width] ]
                                            join = foldr (++) ""
                                            showRow as = join (map show as) ++ "\n"
                                         in join (map showRow cells)

-- | Shift the focused cell by the given displacement
shiftGrid :: Grid a -> (Int, Int) -> Grid a
shiftGrid (Store f (x,y)) (shiftX, shiftY) =  Store f (x + shiftX, y + shiftY)
