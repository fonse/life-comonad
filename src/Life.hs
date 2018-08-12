module Life
    ( Cell(..)
    , neighborhood
    , generations
    , rules
    , life
    ) where

import Grid
import Comonad

-- | A simple binary cell.
--
-- Can be either 'On' or 'Off'.
data Cell = On | Off deriving Eq

instance Show Cell where
  show On = "x"
  show Off = " "

-- | List of neighbors to the focused cell in the 'Grid' in clockwise order, starting from top-left.
neighborhood :: Grid a -> [a]
neighborhood grid = extract <$> ($ grid) <$> [nw, n, ne, e, se, s, sw, w]

-- | Infinite list of all generations obtained by successively applying a set of rules to an initial 'Grid'.
generations :: (Grid a -> a) -> Grid a -> [Grid a]
generations f = iterate $ extend f

-- | Rules for Conway's Game of Life.
--
--   * If a cell has less than 2 neighbors, it dies of loneliness.
--   * If a cell has four or more neighbors, it dies of overcrowding.
--   * If a cell has 2 or 3 neighbors, it continues to live.
--   * If an empty cell has exactly three neighbors, a new cell is born.
rules :: Grid Cell -> Cell
rules grid = let val = extract grid
                 liveNeighbors = length $ filter (== On) $ neighborhood grid
             in  rules' val liveNeighbors

-- | New value for a 'Cell' given its current value and the number of live neighbors.
rules' On 2 = On
rules' _  3 = On
rules' _  _ = Off

-- | Infinite list of generations for a Life game given an initial 'Grid'.
life :: Grid Cell -> [Grid Cell]
life = generations rules
