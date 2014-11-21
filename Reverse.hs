module Reverse (Reverse (Reverse), getReverse) where

import Data.Foldable as F
import Data.Traversable

newtype Reverse f a = Reverse {getReverse :: f a}

instance Functor f => Functor (Reverse f) where
  fmap f (Reverse x) = Reverse (fmap f x)

instance Foldable f => Foldable (Reverse f) where
  foldr f z (Reverse x) = Prelude.foldl (flip f) z (F.toList x)
  
instance Traversable f => Traversable (Reverse f) where
  traverse f (Reverse x) = fmap Reverse (traverse f x)

