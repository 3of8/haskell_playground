{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import MaximumMatching (maximumMatching)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Ord
import Data.List (nub, (\\), maximumBy)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as M

import Test.QuickCheck hiding (label)
import Test.QuickCheck.Gen


isMatching :: Graph gr => gr a b -> [Edge] -> Bool
isMatching g m = null (m \\ edges g) && distinct (concatMap (\(a,b) -> [a,b]) m)
  where distinct xs = nub xs == xs
  
isMaximalMatching :: Graph gr => gr a b -> [Edge] -> Bool
isMaximalMatching g m = isMatching g m && and [a `elem` ns || b `elem` ns | (a,b) <- edges g]
  where ns = nub $ concatMap (\(a,b) -> [a,b]) m

maximalMatchings :: Graph gr => gr a b -> [[Edge]]
maximalMatchings g = f [([], [(a,b) | (a,b) <- edges g, a <= b])] []
  where  f [] []           = []
         f [] ms'          = f ms' []
         f ((m,[]):ms) ms' = m : f ms ms'
         f ((m,es):ms) ms' = f ms (m' ++ ms')
           where m' = do ((a,b), es') <- suffixes es
                         return ((a,b):m, [(c,d) | (c,d) <- es', a /= c, b /= c, a /= d, b /= d])
         suffixes []       = []
         suffixes (x:xs)   = (x,xs) : suffixes xs

maximumMatchingNaive :: Graph gr => gr a b -> [Edge]
maximumMatchingNaive g = maximumBy (comparing length) (maximalMatchings g)

isMaximumMatching :: Graph gr => gr a b -> [Edge] -> Bool
isMaximumMatching g m = isMatching g m && length m == length (maximumMatchingNaive g)


class Eq a => Label a where
  label :: Integer -> a
  
instance Label () where
  label _ = ()

instance (Label a, Label b) => Arbitrary (Gr a b) where
  arbitrary = 
    do es <- fmap (filter (uncurry (/=))) arbitrary :: Gen [(Integer, Integer)]
       let ns = nub (concatMap (\(a,b) -> [a,b]) es)
       let nodes = newNodes (length ns) (empty :: Gr a b)
       let lnodes = zip nodes (map label ns) :: [LNode a]
       let m = M.fromList (zip ns nodes)
       let ledges = [(fromJust $ M.lookup x m, fromJust $ M.lookup y m, label i) | ((x,y), i) <- zip es [0..]]
       return $ undir (mkGraph lnodes ledges)
  shrink = shrinkNothing


propIsMaximal :: Property
propIsMaximal = property $ \g -> isMaximalMatching g (maximumMatching (g :: Gr () ()))

propIsMaximum :: Property
propIsMaximum = property $ \g -> isMaximumMatching g (maximumMatching (g :: Gr () ()))

main = 
  do quickCheckWith stdArgs {maxSuccess = 10000} propIsMaximal
     quickCheckWith stdArgs {maxSize = 30, maxSuccess = 2000} propIsMaximum

