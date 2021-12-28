module AOC.AStar (
    astar
  , Vertex
  , VertexCosts
  , Path
  , Heuristic
  , Graph
  , Cost) where

import qualified Data.HashPSQ as PQ (HashPSQ, insert, empty, minView, null, toList)
import qualified Data.Map as M (Map, empty, insert, lookup)
import Data.Hashable (Hashable)

type Vertex = (Int,Int)
type VertexCosts = M.Map Vertex Cost

type PrioQ k p = PQ.HashPSQ k p k

type Cost = Int
type Graph = Vertex -> [(Vertex, Cost)]
type Heuristic = Vertex -> Cost
type Path = ([Vertex], Cost)

end :: Path -> Vertex
end = head . fst

extract :: Path -> Path
extract (vs,c) = (reverse vs, c)

insertQ :: (Hashable a, Ord a, Ord p) => a -> p -> PrioQ a p -> PrioQ a p
insertQ value prio = PQ.insert value prio value

deleteQ :: (Hashable a, Ord a, Ord p) => PrioQ a p -> ((a, p), PrioQ a p)
deleteQ pq = case PQ.minView pq of
  Nothing -> error "deleteQ on empty queue"
  Just (_, prio, value, pq') -> ((value, prio), pq')

removeQ :: (Hashable a, Ord a, Ord p) => PrioQ a p -> (a, PrioQ a p)
removeQ pq = (x,pq') where ((x,_), pq') = deleteQ pq

emptyQ :: PrioQ k p
emptyQ = PQ.empty

nullQ :: PrioQ k p -> Bool
nullQ = PQ.null

addListQ :: (Foldable t, Hashable a, Ord a,  Ord p) => t (a, p) -> PrioQ a p -> PrioQ a p
addListQ items pq = foldr (\(k, p) acc -> insertQ k p acc) pq items

toListQ :: (Hashable a, Ord a, Ord p) => PrioQ a p -> [(a, p)]
toListQ = map (\(k, p, _) -> (k, p)) . PQ.toList

succs :: Graph -> Heuristic -> Path -> [(Path,Cost)]
succs g h (u:vs, c) = [((v:u:vs, c+d), c+d+h v) | (v,d) <- g u]
succs _ _ ([], _) = []

astar :: Graph -> Heuristic -> (Vertex -> Bool) -> Vertex -> Maybe Path
astar g h isGoal source = asearch M.empty start
  where
    start = insertQ ([source], 0) (h source) emptyQ
    asearch vcmap ps
      | nullQ ps = Nothing
      | isGoal (end p) = Just (extract p)
      | better p vcmap = asearch vcmap qs
      | otherwise = asearch (add p vcmap) rs
      where
        (p, qs) = removeQ ps
        rs = addListQ (succs g h p) qs

    better :: Path -> VertexCosts -> Bool
    better (v:_, c) vcmap =
      query (M.lookup v vcmap)
      where
        query Nothing = False
        query (Just c') = c' <= c
    better _ _ = False

    add :: Path -> VertexCosts -> VertexCosts
    add (v:_, c) vcmap = M.insert v c vcmap
    add _ vcmap = vcmap
