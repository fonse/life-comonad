module Comonad
    ( Comonad(..)
    ) where

{- |
Simple implementation of a comonad.

The definitios for 'extract' and 'duplicate' must satisfy these laws:

@
'extract' . 'duplicate'      = 'id'
'fmap' 'extract' . 'duplicate' = 'id'
'duplicate' . 'duplicate'    = 'fmap' 'duplicate' . 'duplicate'
@
-}
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
