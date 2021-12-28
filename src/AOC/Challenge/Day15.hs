{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module AOC.Challenge.Day15 (day15a, day15b) where

import AOC.Solver ( (:~>)(..) )
import Data.Bifunctor ( Bifunctor(second) )
import qualified Data.Map as M (fromList, keys, (!))
import AOC.AStar ( astar, Path, Cost, VertexCosts, Vertex )

type Grid = (VertexCosts, Vertex, Vertex)

costMap :: Grid -> VertexCosts
costMap (cm,_,_) = cm

maxs :: Grid -> Vertex
maxs (_,v,_) = v

goal :: Grid -> Vertex
goal (_,_,g) =g

solve :: Grid -> Maybe Path
solve g =
  astar graph heuristic (== goal g) (0, 0)
  where
    (gx,gy) = goal g
    heuristic = \(x,y) -> (gx - x) + (gy - y)
    graph v = nextTo v g

parse :: Int -> String -> Grid
parse factor s =
  let verticies  =
        map (second (subtract 48 . fromEnum))                   -- interpret the values as natural numbers
        . concatMap (\(y, xs) -> map (\(x,v) -> ((x,y), v)) xs)   -- rearrange coords into vertex and flatten
        . zip [0..]                                               -- then prepend the y coord to each line
        . map (zip [0..])                                         -- prepend the x coordinate to each digit
        . lines $ s                                               -- for each line
        in
  let vertMap = M.fromList verticies in
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
  let (maxx,maxy) = goal g in
  putStrLn 
  . unlines 
  . map (\y -> concatMap (\x -> show . snd . mGet g $ (x,y)) [0..maxx]) $ [0..maxy]

day15x :: Int -> Grid :~> Cost
day15x n = MkSol { sParse = Just . parse n, sShow = show, sSolve = fmap snd . solve }

day15a :: Grid :~> Cost
day15a = day15x 1

day15b :: Grid :~> Cost
day15b = day15x 5
