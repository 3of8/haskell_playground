{-|
Module      : Test
Description : QuickCheck tests for MaximumMatching
Licence     : LGPL-2.1
Maintainer  : Manuel Eberl <last name + m _at_ in.tum.de>
Stability   : experimental

This module provides some QuickCheck tests for the functions related to maximal and maximum matchings.

-}
module Main where

import Data.Graph.Inductive.Query.MaximumMatching

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Ord
import Data.List (nub, maximumBy, sort)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Test.QuickCheck hiding (label)
import Test.QuickCheck.Gen

maximalMatchingsNaive :: Graph gr => gr a b -> [[Edge]]
maximalMatchingsNaive g = aux []
  where augment m = [(a,b) : m | (a,b) <- edges g, all (\(c,d) -> a /= c && a /= d && b /= c && b /= d) m]
        aux m = case augment m of
                  [] -> [m]
                  ms -> concatMap aux ms

maximumMatchingNaive :: Graph gr => gr a b -> [Edge]
maximumMatchingNaive g = maximumBy (comparing length) (maximalMatchings g)

isMaximumMatchingNaive :: Graph gr => gr a b -> [Edge] -> Bool
isMaximumMatchingNaive g m = isMatching g m && length m == length (maximumMatchingNaive g)


genGraph = 
    do es <- fmap (filter (uncurry (/=))) arbitrary :: Gen [(Integer, Integer)]
       let ns = nub (concatMap (\(a,b) -> [a,b]) es)
       let nodes = newNodes (length ns) (empty :: Gr () ())
       let lnodes = zip nodes (repeat ()) :: [LNode ()]
       let m = M.fromList (zip ns nodes)
       let ledges = [(fromJust $ M.lookup x m, fromJust $ M.lookup y m, ()) | ((x,y), i) <- zip es [0..]]
       return (mkGraph lnodes ledges)

propMaximalMatchingsComplete :: Property
propMaximalMatchingsComplete = 
    forAll genGraph $ \g -> let ms = S.fromList (map sort (maximalMatchings g)) 
                            in all (\m -> sort m `S.member` ms) (maximalMatchingsNaive g)
    
propMaximalMatchingsAreMaximal :: Property
propMaximalMatchingsAreMaximal = forAll genGraph $ \g -> all (isMaximalMatching g) (maximalMatchings (g :: Gr () ()))

propMaximumMatchingIsMaximal :: Property
propMaximumMatchingIsMaximal = forAll genGraph $ \g -> isMaximalMatching g (maximumMatching (g :: Gr () ()))

propMaximumMatchingIsMaximum :: Property
propMaximumMatchingIsMaximum = forAll genGraph $ \g -> isMaximumMatchingNaive g (maximumMatching (g :: Gr () ()))

main = 
  do putStrLn "Test 1: maximalMatchings computes all maximal matchings"
     quickCheckWith stdArgs {maxSize = 15, maxSuccess = 2000} propMaximalMatchingsComplete
     putStrLn "Test 2: maximalMatchings computes only maximal matchings"
     quickCheckWith stdArgs {maxSize = 20, maxSuccess = 10000} propMaximalMatchingsAreMaximal
     putStrLn "Test 3: maximumMatchings computes a maximal matching"
     quickCheckWith stdArgs {maxSuccess = 10000} propMaximumMatchingIsMaximal
     putStrLn "Test 4: maximumMatchings computes a maximum matching"
     quickCheckWith stdArgs {maxSize = 30, maxSuccess = 2000} propMaximumMatchingIsMaximum

