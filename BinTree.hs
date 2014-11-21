{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module BinTree (
    BinTree (Node, Tip), 
    TraversalType (PreOrder, InOrder, PostOrder),
    empty, singleton, null
  ) where

import Prelude hiding (foldr, null)
import Data.Foldable
import Data.Traversable
import Control.Applicative hiding (empty)

data TraversalType = PreOrder | InOrder | PostOrder
  
data BinTree :: TraversalType -> * -> * where
  Node :: BinTree t x -> x -> BinTree t x -> BinTree t x
  Tip :: BinTree t x

empty :: BinTree p a
empty = Tip

singleton :: a -> BinTree p a
singleton x = Node Tip x Tip

null :: BinTree p a -> Bool
null Tip = True
null _   = False

changeTraversalType :: BinTree p a -> BinTree p' a
changeTraversalType Tip = Tip
changeTraversalType (Node l x r) = Node (changeTraversalType l) x (changeTraversalType r)

instance Functor (BinTree t) where
  fmap _ Tip = Tip
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable (BinTree PreOrder) where
  foldr _ z Tip = z
  foldr f z (Node l x r) = f x (foldr f (foldr f z r) l)
  
instance Foldable (BinTree InOrder) where
  foldr _ z Tip = z
  foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l

instance Foldable (BinTree PostOrder) where
  foldr _ z Tip = z
  foldr f z (Node l x r) = foldr f (foldr f (f x z) r) l

traverseTree :: Applicative f => (a -> f b) -> BinTree t a -> f (BinTree t b)
traverseTree f Tip = pure Tip
traverseTree f (Node l x r) = Node <$> traverseTree f l <*> f x <*> traverseTree f r

instance Traversable (BinTree PreOrder) where
  traverse = traverseTree

instance Traversable (BinTree InOrder) where
  traverse = traverseTree

instance Traversable (BinTree PostOrder) where
  traverse = traverseTree

