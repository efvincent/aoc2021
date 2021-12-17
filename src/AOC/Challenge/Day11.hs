{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AOC.Challenge.Day11 (day11a, day11b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, (!), fromList, keys, adjust, insert, elems)
import Data.Set as S (Set, empty, insert, member)

type Point = (Int,Int)
type Grid = Map Point Int
type Puzzle = (Grid, Point)

parse :: String -> Puzzle
parse s = (g, maximum . keys $ g)
  where
    g = fromList
        . map       (\(y,(v,x)) -> ((x,y), v))
        . concatMap (\(y,l)     -> [0..] `zip` map ((,y) . subtract 48 . fromEnum) l)
        . ([0..] `zip`)
        . lines $ s

-- | returns neighbors of a point, including diagonal, excluding points that 
-- fall outside of the puzzle
neighbors :: Puzzle -> Point -> [Point]
neighbors (_,(mx,my)) (x,y) =
  [(a,b) | a <- [(x-1)..(x+1)]
         , b <- [(y-1)..(y+1)]
         , (a,b) /= (x,y)
         , a >= 0 && a <= mx
         , b >= 0 && b <= my ]

allPoints :: Puzzle -> [Point]
allPoints (_,(mx,my)) = [(x,y) | x <- [0..mx], y <- [0..my]]

-- | Runs one generation on the puzzle, returning the number of flashes that
-- occurred and the new puzzle
step :: Puzzle -> (Int,Puzzle)
step puz@(g,m) =
  let (flashes, grid) = go empty steppedGrid in
  (flashes, (grid,m))
  where
    steppedGrid = (+1) <$> g  -- Increment all values in grid
    allPts = allPoints puz

    -- | returns true if the the point has a value > 9 and is not in 
    -- the set of points that have already flashed this step
    isFlashing :: Set Point -> Grid -> Point -> Bool
    isFlashing flashed grid point = (grid!point) > 9 && not (member point flashed)

    -- | Sets all values > 9 to zero
    zero :: Grid -> Grid
    zero grid = 
      foldr (\p acc -> if acc!p <= 9 then acc else M.insert p 0 acc) grid allPts

    -- | visit each point, if it is due to flash and hasn't yet, increment all its
    -- neighbors and add it to the set of points that have flashed this step
    flashPoints :: Set Point -> Grid -> [Point] -> (Set Point, Grid)
    flashPoints flashed grid [] = (flashed,grid)
    flashPoints flashed grid (point:pts) | isFlashing flashed grid point =
      let grid' = foldr (adjust (+1)) grid (neighbors puz point) in
      flashPoints (S.insert point flashed) grid' pts
    flashPoints flashed grid (_:pts) = flashPoints flashed grid pts

    -- | flash all the points that are due to flash. If nothing flashed, then return
    -- the accumulated number of flashes and the zero'd out updated grid. Otherwise, 
    -- accumulate the number of flashes and repeat
    go :: Set Point -> Grid -> (Int, Grid)
    go flashed grid =
      let (flashed', grid') = flashPoints flashed grid allPts in
      if length flashed' > length flashed  
      then go flashed' grid'
      else (length flashed, zero grid')

-- | Run the requested number of steps, accumulating the flashes in each step, returning
-- the total number of flashes across all steps and the updated puzzle
steps :: Int -> Puzzle -> (Int, Puzzle)
steps i p = foldr (\_ (c,puz) -> let (c',puz') = step puz in (c+c', puz')) (0,p) [1..i]

-- | Run a step, if any of the values are non-zero, run another step, otherwise return the
-- number of steps it took to reach a state where all values are zero
solve2 :: Puzzle -> Int
solve2 = go 1
  where
    go n puz =
      let (_,puz'@(g,_)) = step puz in
      if any (> 0) (elems g)
      then go (n+1) puz'
      else n 

day11x :: Show a => (Puzzle -> a) -> Puzzle :~> a
day11x fn = MkSol { sParse = Just . parse, sShow = show, sSolve = Just . fn }

day11a :: Puzzle :~> Int
day11a = day11x $ fst . steps 100

day11b :: Puzzle :~> Int
day11b = day11x solve2