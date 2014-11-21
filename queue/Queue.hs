{-# LANGUAGE DeriveDataTypeable #-}
module Queue 
  (Queue, empty, null, singleton, size, enqueue, enqueueList, dequeue, peek, take, drop, splitAt, fromList, toList)
  where

import Prelude hiding (take, drop, splitAt, null)
import Data.List (genericLength)
import Data.Foldable as F hiding (toList)
import Data.Traversable as T
import Data.Function (on)
import Data.Typeable (Typeable)
import Control.DeepSeq (rnf, NFData)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)

import Test.QuickCheck
import Test.QuickCheck.Gen


data Queue a = Queue Int [a] [a] deriving Typeable

empty :: Queue a
empty = Queue 0 [] []

null :: Queue a -> Bool
null (Queue _ [] []) = True
null _               = False

singleton :: a -> Queue a
singleton x = Queue 1 [] [x]

size :: Queue a -> Int
size (Queue s _ _) = s

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue s xs ys) = Queue (s + 1) (x:xs) ys

enqueueList :: [a] -> Queue a -> Queue a
enqueueList zs (Queue s xs ys) = Queue (s + length zs) (zs ++ xs) ys

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue s xs (y:ys)) = Just (y, Queue (s - 1) xs ys)
dequeue (Queue s xs [])     = case reverse xs of
                                [] -> Nothing 
                                (x:xs') -> Just (x, Queue (s - 1) [] xs')

peek :: Queue a -> Maybe a
peek (Queue _ [] [])     = Nothing
peek (Queue _ _  (y:ys)) = Just y
peek (Queue _ xs [])     = Just (last xs)

take :: Int -> Queue a -> [a]
take 0 q = []
take n q = case dequeue q of
             Nothing     -> []
             Just (x,q') -> x : take (n - 1) q'

drop :: Int -> Queue a -> Queue a
drop 0 q = q
drop n q = case dequeue q of
             Nothing     -> q
             Just (x,q') -> drop (n - 1) q'
             
splitAt :: Int -> Queue a -> ([a], Queue a)
splitAt 0 q = ([], q)
splitAt n q = case dequeue q of
                Nothing     -> ([], q)
                Just (x,q') -> case splitAt (n - 1) q' of (xs, q'') -> (x:xs, q'')

fromList :: [a] -> Queue a
fromList xs = Queue (length xs) [] xs

toList :: Queue a -> [a]
toList (Queue _ xs ys) = ys ++ reverse xs

instance Eq a => Eq (Queue a) where
  (==) = (==) `on` toList
  
instance Ord a => Ord (Queue a) where
  compare = compare `on` toList
  
instance NFData a => NFData (Queue a) where
  rnf (Queue s xs ys) = rnf s `seq` rnf xs `seq` rnf ys

instance Show a => Show (Queue a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)
  
instance Read a => Read (Queue a) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)

instance Functor Queue where
  fmap f (Queue s xs ys) = Queue s (map f xs) (map f ys)
  
instance Foldable Queue where
  foldr f z (Queue _ xs ys)   = F.foldr f (F.foldl (flip f) z xs) ys
  
instance Traversable Queue where
  sequenceA (Queue s xs ys)  = Queue s <$> sequenceA xs <*> sequenceA ys
  traverse f (Queue s xs ys) = Queue s <$> traverse f xs <*> traverse f ys
  mapM f   = liftM fromList . T.mapM f . toList
  sequence = liftM fromList . T.sequence . toList

instance Arbitrary a => Arbitrary (Queue a) where
  arbitrary = fromList <$> arbitrary
  shrink    = map fromList . shrink . toList

