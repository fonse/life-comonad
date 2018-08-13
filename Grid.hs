{-# LANGUAGE RankNTypes #-}

module Grid
    ( Grid(..)
    , Store(..)
    , memorize
    , showGrid
    , shiftGrid
    , memorizeGrid
    ) where

import Comonad
import qualified Data.MemoCombinators as Memo

-- | Generic comonad dual to 'State' monad.
-- Essentially a representable functor along with an instance of its index type.
data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap g (Store f s) = Store (g . f) s

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

-- | Memorize a Store using 'MemoCombinators' to improve performance.
memorize :: Memo.Memo s -> Store s a -> Store s a
memorize memo (Store f s) = Store (memo f) s

{- |
An infinite square grid focused on a specific cell.

Represented by a function @(Int,Int) -> a@ which indicates the value of the grid for all points, along with a pair of coordinates to use as the focused cell.

Positive directions are downwards for the first coordinate and to the right for the second one.

For example:

@
gliderGrid = Store f (0,0) where
  f (0,1) = 'x'
  f (1,2) = 'x'
  f (2,0) = 'x'
  f (2,1) = 'x'
  f (2,2) = 'x'
  f _     = ' '
@
-}
type Grid a = Store (Int,Int) a

-- | Show a section of the grid, spanning the given height and width with the focused cell as the upper-left corner.
showGrid :: Show a => Int -> Int -> Grid a -> String
showGrid height width (Store f (i,j)) = let cells = [ [ f (i', j') | j' <- [j..j+width] ] | i' <- [i..i+height] ]
                                            join = foldr (++) ""
                                            showRow as = join (map show as) ++ "\n"
                                         in join (map showRow cells)

-- | Shift the focused cell by the given displacement
shiftGrid :: Grid a -> (Int, Int) -> Grid a
shiftGrid (Store f (i,j)) (shiftVertical, shiftHorizontal) =  Store f (i + shiftVertical, j + shiftHorizontal)

-- | Memorize applied to the specific case of 'Grid'
memorizeGrid :: Grid a -> Grid a
memorizeGrid = memorize $ Memo.pair Memo.integral Memo.integral
