{-# LANGUAGE ScopedTypeVariables #-}
module Exercise_6 where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Query.MaximumMatching
import Data.Maybe (fromJust)

type Tutor = String -- Name des Tutors
type Time = Integer -- Zeit kodiert als Integer (der Einfachheit wegen)
type TimeSlot = (Time, Time) -- Anfangs-/Endzeit eines Zeitslots

isOverlapping :: TimeSlot -> TimeSlot -> Bool
isOverlapping (s1, e1) (s2, e2) = max s1 s2 < min e1 e2

-- Gibt alle (ungeordneten) Paare aus Elementen einer Liste zurück
allPairs :: [a] -> [(a,a)]
allPairs []     = []
allPairs (x:xs) = [(x,y) | y <- xs] ++ allPairs xs

-- Baut einen Graphen, dessen Knoten die Zeitslots sind. 2 Zeitslots sind verbunden gdw. sie sich nicht überschneiden.
buildGraph :: forall gr. Graph gr => [TimeSlot] -> gr TimeSlot ()
buildGraph ss = mkGraph ns [(n1,n2,()) | ((n1,s1), (n2,s2)) <- allPairs ns, not (isOverlapping s1 s2)]
  where ns = zip (newNodes (length ss) (empty :: gr TimeSlot ())) ss

-- Berechnet aus einem größten Matching im Graphen eine Lösung für das eigentliche Problem
constructSolution :: Graph gr => [Tutor] -> gr TimeSlot a -> [Edge] -> (Bool, [(Tutor, TimeSlot, TimeSlot)])
constructSolution ts g ms = if length ts > length ms then (False, []) else (True, zipWith f ts ms)
  where f t (n1, n2) = (t, fromJust (lab g n1), fromJust (lab g n2))
        
findMapping :: [Tutor] -> [TimeSlot] -> (Bool, [(Tutor, TimeSlot, TimeSlot)])
findMapping ts ss = constructSolution ts g (maximumMatching g)
  where g = buildGraph ss :: Gr TimeSlot ()

{-TTEW-}

