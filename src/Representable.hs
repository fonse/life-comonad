{-# LANGUAGE TypeFamilies #-}

module Representable
    ( Representable(..)
    ) where

-- | A 'functor' @f@ is representable if @f a@ isomorphic to @x -> a@ for some @x@.
class Representable f where
   type Key f :: *
   tabulate :: (Key f -> x) -> f x
   index    :: f x -> Key f -> x
