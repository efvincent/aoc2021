module AOC.Challenge.Day09 (day09a, day09b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, (!), fromList, keys)
import Data.Set as S (Set, empty, insert, toList, member)
import AOC.Prelude (catMaybes)
import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))

type Point = (Int,Int)

data Location = Location
  { _pos :: Point
  , _value :: Int }

instance Ord Location where
  compare l1 l2 = compare (_pos l1) (_pos l2)

instance Eq Location where
  (==) l1 l2 = _pos l1 == _pos l2

type Grid = M.Map Point Location

data Puzzle = Puzzle
  { _grid :: Grid
  , _maxx :: Int
  , _maxy :: Int }

parse :: String -> Puzzle
parse s =
  let (mx,my) = maximum . M.keys $ g in
  Puzzle g mx my
  where
    g = M.fromList
        . map       (\(x,(v,y)) -> ((x,y), Location (x,y) v))
        . concatMap (\(y,l)     -> [0..] `zip` map ((,y) . subtract 48 . fromEnum) l)
        . ([0..] `zip`)
        . lines $ s

-- | filter all indexes down to just those who's neighbors' values are all greater,
-- then map those points to their values + 1, and sum that list
solve1 :: Puzzle -> Int
solve1 Puzzle{_grid=g, _maxx=mx, _maxy=my} =
  sum . map ((+ 1) . _value . get g) . filter predicate $ idxs
  where
    get p coord =  (! coord) p
    idxs = [(x,y) | x <- [0..mx], y <- [0..my]]
    predicate p@(x,y) =
      let gval = _value . get g in
      let v = gval p in
      and
        [ x <= 0  || gval (x-1, y) > v
        , x >= mx || gval (x+1, y) > v
        , y >= my || gval (x, y+1) > v
        , y <= 0  || gval (x, y-1) > v]

-- | perform a flood fill on each point. The flood fill determines (cheaply) if the
-- point is not fillable, either because it's value is 9 or it has already been filled.
-- Map the resulting set of @List Location@ (basins) to their lengths, sort them descending,
-- take the product of the top 3. Note the use of @sortBy (comparing Down)@ is significantly
-- better performing than @sortOn Down@, especially in the lazy case like this one where we're
-- only taking the top three largest from the sorted list. See a detailed explanation at 
-- https://ro-che.info/articles/2016-04-02-descending-sort-haskell
solve2 :: Puzzle -> Int
solve2 puz@Puzzle{_maxx=mx, _maxy=my} =
  let idxs = [(x,y) | x <- [0..mx], y <- [0..my] ] in
  let (_, basins) = foldr (floodFill puz) (S.empty, []) idxs in
  product . take 3 . sortBy (comparing Down) . map length $ basins

-- | collects all the points in a "basin" - effectively the same logic as a simple flood 
-- fill - by recursively inserting each valid point, and then repeating with each neighbor
-- of each valid point, until a point has no neighbors. A @Set Point@ is used to track
-- points that have been seen. In the use of this to determine multiple basins, the set
-- of seen points is threaded through all the calls to @floodFill@
floodFill :: Puzzle -> Point -> (Set Point, [[Point]]) -> (Set Point, [[Point]])
floodFill Puzzle{_grid=g, _maxx=mx, _maxy=my} fillPoint (seen, basins) =
  let (seen', cur) = go fillPoint (seen,S.empty) in
  let basins' = if null cur then basins else S.toList cur: basins in
  (seen', basins')
  where
    neighbors (x,y) =
      catMaybes
      [ if y > 0  then Just (x, y-1) else Nothing
      , if x > 0  then Just (x-1, y) else Nothing
      , if x < mx then Just (x+1, y) else Nothing
      , if y < my then Just (x, y+1) else Nothing ]

    go :: Point -> (Set Point, Set Point) -> (Set Point, Set Point)
    go pt (allFps, fps) | _value (g M.! pt) == 9 || S.member pt allFps = (allFps, fps)
    go pt (allFps, fps) =
      let sets = (S.insert pt allFps, S.insert pt fps) in
      let pts = neighbors pt in
      foldr go sets pts

day9x :: Show a => (Puzzle -> a) -> Puzzle :~> a
day9x fn = MkSol { sParse = Just . parse, sShow = show, sSolve = Just . fn }

day09a :: Puzzle :~> Int
day09a = day9x solve1

day09b :: Puzzle :~> Int
day09b = day9x solve2
