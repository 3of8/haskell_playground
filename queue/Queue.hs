module Queue 
  (Queue, empty, null, singleton, size, enqueue, enqueueList, dequeue, peek, take, drop, splitAt, fromList, toList)
  where

import Prelude hiding (take, drop, splitAt, null)
import Data.List (genericLength)

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

null :: Queue a -> Bool
null (Queue [] []) = True
null _             = False

singleton :: a -> Queue a
singleton x = Queue [] [x]

size :: Queue a -> Integer
size (Queue xs ys) = genericLength xs + genericLength ys

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs ys) = Queue (x:xs) ys

enqueueList :: [a] -> Queue a -> Queue a
enqueueList zs (Queue xs ys) = Queue (zs ++ xs) ys

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue xs (y:ys)) = Just (y, Queue xs ys)
dequeue (Queue xs [])     = case reverse xs of
                              [] -> Nothing 
                              (x:xs') -> Just (x, Queue [] xs')

peek :: Queue a -> Maybe a
peek (Queue [] [])     = Nothing
peek (Queue _  (y:ys)) = Just y
peek (Queue xs [])     = Just (last xs)

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
fromList xs = Queue xs []

toList :: Queue a -> [a]
toList (Queue xs ys) = ys ++ reverse xs

