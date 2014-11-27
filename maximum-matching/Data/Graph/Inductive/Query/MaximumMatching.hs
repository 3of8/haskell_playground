{-|
Module      : Matchings
Description : Provides some functions on matchings in graphs
Licence     : LGPL-2.1
Maintainer  : Manuel Eberl <last name + m _at_ in.tum.de>
Stability   : experimental

This module provides some algorithms related to matchings in graphs, most notably an implementation of 
Edmond's blossom algorithm, which computes maximum matchings for graphs. 

Definitions:

  * A /matching/ is a subset of the edges of a graph such that no two edges in it are incident to the same node. 
  
  * A /maximal matching/ is a matching that cannot be made any larger, i.e. no additional
edge can be added to it without violating the property of node-disjoint edges.

  * A /maximum matching/ is a matching such that no other matching contains more edges.

In this list, the given notions are strictly increasing in strength. In particular, note that every maximum matching 
is also maximal, but not every maximal matching is a maximum one.

Our implementation of Edmond's blossom algorithm is an adaptation of the Java implementation in JGraphT: 
<https://github.com/jgrapht/jgrapht/blob/master/jgrapht-core/src/main/java/org/jgrapht/alg/EdmondsBlossomShrinking.java>
-}

module Data.Graph.Inductive.Query.MaximumMatching 
    (isMatching, isMaximalMatching, isMaximumMatching, maximalMatchings, maximumMatching) where

import Control.Monad
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (nub, (\\))

type Matching = Map Node Node

-- | Inserts all the elements of a list into a set.
insertList :: Ord a => [a] -> Set a -> Set a
insertList xs s = S.union s (S.fromList xs)

-- | Updates a map such that all keys from the list point to the given value.
updateList :: Ord k => [k] -> v -> Map k v -> Map k v
updateList ks v m = M.union (M.fromList [(k,v) | k <- ks]) m

-- | A lookup in the List monad instead of Maybe.
lookup' :: Ord k => k -> Map k v -> [v]
lookup' k m = maybe [] (\x -> [x]) (M.lookup k m)

-- | Determines whether a given set of edges is a matching.
isMatching :: Graph gr => gr a b -> [Edge] -> Bool
isMatching g m = null (m \\ edges g) && distinct (concatMap (\(a,b) -> [a,b]) m)
  where distinct xs = nub xs == xs

-- | Determines whether a given set of edges is a maximal matching, i.e. a matching that cannot be 
--   extended by adding another edge.
isMaximalMatching :: Graph gr => gr a b -> [Edge] -> Bool
isMaximalMatching g m = isMatching g m && and [a `elem` ns || b `elem` ns | (a,b) <- edges g]
  where ns = nub $ concatMap (\(a,b) -> [a,b]) m

-- | Computes all maximal matchings in a graph.
maximalMatchings :: Graph gr => gr a b -> [[Edge]]
maximalMatchings g = f [([], edges g)] []
  where  -- First parameter:  list of matchings with the set of edges that could still be added to it
         -- Second parameter: accumulator of matchings that have already been processed in this pass, but can still be 
         --                   enlarged in the next pass.
         -- When we have a matching that cannot be enlarged with an additional edge, we return it; otherwise, 
         -- we put in all the ways in which it can be enlarged for the next pass.
         f :: [([Edge], [Edge])] -> [([Edge], [Edge])] -> [[Edge]]
         f [] []           = []
         f [] ms'          = f ms' []
         f ((m,[]):ms) ms' = if isMaximalMatching g m then m : f ms ms' else f ms ms'
         f ((m,es):ms) ms' = f ms (m' ++ ms')
           where m' = do ((a,b), es') <- suffixes es
                         return ((a,b):m, [(c,d) | (c,d) <- es', a /= c, b /= c, a /= d, b /= d])
         -- Chooses an element of the list and returns that element and all its successors.
         suffixes :: [c] -> [(c, [c])]
         suffixes []       = []
         suffixes (x:xs)   = (x,xs) : suffixes xs

-- | Determines whether the given set of edges is a maximum matching.
isMaximumMatching :: Graph gr => gr a b -> [Edge] -> Bool
isMaximumMatching g m = isMatching g m && length m == length (maximumMatching g)


-- | Computes a maximum matching of the given graph using Edmond's blossom algorithm.
maximumMatching :: Graph gr => gr a b -> [Edge]
maximumMatching g = [if (v,w) `S.member` edgeSet then (v,w) else (w,v) 
                         | v <- nodes g, w <- lookup' v matching, v <= w]
  where edgeSet = S.fromList (edges g)
        matching = foldl (matchNode g) M.empty (nodes g)

matchNode :: Graph gr => gr a b -> Matching -> Node -> Matching
matchNode g matching root
  | root `M.member` matching = matching
  | otherwise = constructMatching $ 
                    matchNode' ([root], S.singleton root, M.empty, M.fromList [(i,i) | i <- nodes g])
  where constructMatching :: (Matching, Maybe Node) -> Matching
        constructMatching (p, v) = 
          let constructMatching' matching Nothing  = matching
              constructMatching' matching (Just v) = 
                  let w = fromJust (M.lookup v p) 
                  in  constructMatching' (M.insert w v (M.insert v w matching)) (M.lookup w matching)
          in  constructMatching' matching v

        matchNode' :: ([Node], Set Node, Map Node Node, Map Node Node) -> (Map Node Node, Maybe Node)
        matchNode' (q, used, p, base) =
          case q of
            []   -> (p, Nothing)
            v:q' -> either id matchNode' (foldM (go v) (q',used,p,base) (neighbors g v))
        
        go :: Node -> ([Node], Set Node, Map Node Node, Map Node Node) -> Node -> 
                  Either (Map Node Node, Maybe Node) ([Node], Set Node, Map Node Node, Map Node Node)
        go v (q, used, p, base) w
          | M.lookup v base     == M.lookup w base = Right (q, used, p, base)
          | M.lookup v matching == Just w          = Right (q, used, p, base)
          | w == root || maybe False (`M.member` p) (M.lookup w matching) =
              let curBase       = lca base p v w
                  (blossom, p') = markPath base w curBase v (markPath base v curBase w (S.empty, p))
                  xs            = [x | x <- nodes g, y <- lookup' x base, y `S.member` blossom]
                  xs'           = [x | x <- xs, x `S.notMember` used]
              in  Right (xs' ++ q, insertList xs' used, p', updateList xs curBase base)
          | w `M.notMember` p =
              let p' = M.insert w v p
              in  case M.lookup w matching of
                    Nothing -> Left (p', Just w)
                    Just w' -> Right (w':q, S.insert w' used, p', base)
          | otherwise = Right (q, used, p, base)

        markPath :: Map Node Node -> Node -> Node -> Node -> (Set Node, Map Node Node) -> 
                        (Set Node, Map Node Node)
        markPath base v b c (blossom, p)
          | M.lookup v base == Just b = (blossom, p)
          | otherwise =
              let w  = fromJust $ M.lookup v matching
                  bv = fromJust $ M.lookup v base
                  bw = fromJust $ M.lookup w base
                  v' = fromJust $ M.lookup w p
              in  markPath base v' b w (S.insert bw (S.insert bv blossom), M.insert v c p)
        
        lca :: Map Node Node -> Map Node Node -> Node -> Node -> Node
        lca base p a b = g b
          where f seen x =
                  let x' = fromJust $ M.lookup x base
                      seen' = S.insert x' seen
                  in  case M.lookup x' matching of
                        Nothing -> seen'
                        Just y  -> f seen' (fromJust (M.lookup y p))
                seen = f S.empty a
                g x =
                  let x' = fromJust $ M.lookup x base
                  in  if x' `S.member` seen then
                        x'
                      else
                        g (fromJust (M.lookup (fromJust (M.lookup x' matching)) p))

