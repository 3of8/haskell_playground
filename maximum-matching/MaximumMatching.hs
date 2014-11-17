{-# LANGUAGE ScopedTypeVariables #-}
module MaximumMatching (maximumMatching) where

import Control.Monad
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

type Matching = Map Node Node

insertList :: Ord a => [a] -> Set a -> Set a
insertList xs s = S.union s (S.fromList xs)

updateList :: Ord k => [k] -> v -> Map k v -> Map k v
updateList ks v m = M.union (M.fromList [(k,v) | k <- ks]) m

lookup' :: Ord k => k -> Map k v -> [v]
lookup' k m = maybe [] (\x -> [x]) (M.lookup k m)


maximumMatching :: Graph gr => gr a b -> [Edge]
maximumMatching g = [(v,w) | v <- nodes g, w <- lookup' v matching, v <= w]
  where matching = foldl (matchNode g) M.empty (nodes g)

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

