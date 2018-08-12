# Conway's Game of Life with comonads
Inspired by a comment made by Bartosz Milewski during one of his [amazing lectures](https://youtu.be/C5oogxdX_Bo) on category theory.

This game of Life is played on an infinite grid that imlpements the comonad definition. This grid contains the state of every possible cell, but it's also "focused" on a specific cell.

We can define the rules of the game as a function `Grid a -> a` that takes a grid and returns the next state for the focused cell by looking at its neighborhood.

This function has the form of a cokleisli arrow, so we can use the comonad definition to extend this function into a new function `Grid a -> Grid a` which resolves the next generation for the entire grid.
