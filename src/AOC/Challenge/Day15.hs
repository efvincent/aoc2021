{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module AOC.Challenge.Day15 (day15a, day15b) where

import AOC.Solver ( (:~>)(..) )
import qualified Data.HashPSQ as PQ (HashPSQ, insert, empty, minView, null, toList)
import Data.Bifunctor ( Bifunctor(second) )
import qualified Data.Map as M (Map, empty, insert, fromList, keys, (!), lookup)
import Data.Hashable (Hashable)

type Vertex = (Int,Int)
type VertexCosts = M.Map Vertex Cost
type Grid = (VertexCosts, Vertex, Vertex)

costMap :: Grid -> VertexCosts
costMap (cm,_,_) = cm

maxs :: Grid -> Vertex
maxs (_,v,_) = v

goal :: Grid -> Vertex
goal (_,_,g) =g

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

solve1 :: Grid -> Maybe Cost
solve1 g = snd <$> solve g

solve :: Grid -> Maybe Path
solve g =
  astar graph heuristic (== goal g) (0, 0)
  where
    (gx,gy) = goal g
    heuristic = \(x,y) -> (gx - x) + (gy - y)
    graph v = nextTo v g    

parse :: Int -> String -> Grid
parse factor s =
  let verts  =
        map (second (subtract 48 . fromEnum))                   -- interpret the values as natural numbers
        . concatMap (\(y, xs) -> map (\(x,v) -> ((x,y), v)) xs)   -- rearrange coords into vertex and flatten
        . zip [0..]                                               -- then prepend the y coord to each line
        . map (zip [0..])                                         -- prepend the x coordinate to each digit
        . lines $ s                                               -- for each line
        in
  let vertMap = M.fromList verts in
  let (mx,my) = maximum . M.keys $ vertMap in
  (vertMap, (mx,my), (((mx + 1) * factor) - 1, ((my + 1) * factor) - 1))

mGet :: Grid -> Vertex -> (Vertex, Cost)
mGet g v@(x,y) = (v,c)
  where
    (mx,my) = maxs g
    wrap cst 
      | cst <= 9 = cst 
      | otherwise = cst `mod` 9
    c = wrap $                                  -- wrap the risk level between 1..9
      ( costMap g M.! ( x `mod` (mx + 1)        -- find the cost at the "wrapped" coordinate
                      , y `mod` (my + 1) ) ) 
      + (x `div` (mx + 1))                      -- add the extr costs of mulitple x and y coords
      + (y `div` (my + 1))

nextTo :: Vertex -> Grid -> [(Vertex, Cost)]
nextTo (vx,vy) g =
  let mget = snd . mGet g in
  let (mx,my) = goal g in
  [(\v -> (v, mget v)) (x, y)
   | (x, y) <- [(vx - 1, vy), (vx + 1, vy), (vx, vy - 1), (vx, vy + 1)]
   , x >= 0 && x <= mx
   , y >= 0 && y <= my]
   
pGrid :: Grid -> IO ()
pGrid g =
  putStrLn $ unlines ls
  where
    (maxx,maxy) = goal g
    ls = map (\y -> concatMap (\x -> show . snd . mGet g $ (x,y)) [0..maxx]) [0..maxy]

day15x :: Int -> Grid :~> Int
day15x n = MkSol { sParse = Just . parse n, sShow = show, sSolve = solve1 }    

day15a :: Grid :~> Int
day15a = day15x 1

day15b :: _ :~> _
day15b = day15x 5
