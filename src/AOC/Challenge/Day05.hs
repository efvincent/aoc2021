module AOC.Challenge.Day05 (day05a, day05b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, insert, empty, findWithDefault, elems)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)

type Point = (Int,Int)
type PointFrequencies = M.Map Point Int
type Line  = (Point,Point)

-- | true if a line is neither vertical or horizontal
diag :: Line -> Bool
diag ((x1,y1), (x2,y2)) = x1 /= x2 && y1 /= y2

-- | take a string to a point if possible
sToPoint :: String -> Maybe Point
sToPoint s = case splitOn "," s of
  [x,y] -> Just (read x,read y)
  _ -> Nothing

-- | take a string and split it into the two points that make up a line
sToLine :: String -> Maybe Line
sToLine s =
  case splitOn " -> " s of
    [s1,s2] -> do
      p1 <- sToPoint s1
      p2 <- sToPoint s2
      return (p1,p2)
    _ -> Nothing

-- | Parse the input into a list of lines if possible
parse :: String -> Maybe [Line]
parse = Just . reverse . mapMaybe sToLine . lines

-- | List of points described by a line according to puzzle rules
lineToPoints :: Line -> [Point]
lineToPoints ((x1,y1),(x2,y2))
  | x1 == x2 =  [(x1, y ) | y <- [(min y1 y2)..(max y1 y2)]]
  | y1 == y2 =  [(x , y1) | x <- [(min x1 x2)..(max x1 x2)]]
  | otherwise =
    let sx = if x1 > x2 then -1 else 1 in
    let sy = if y1 > y2 then -1 else 1 in
    [(x1 + (offset * sx), y1 + (offset * sy) ) | offset <- [0..(abs (x1-x2))]]

-- | add the points on the given line to the frequency map 
addPointsToFreqMap :: PointFrequencies -> Line -> PointFrequencies
addPointsToFreqMap points line =
  foldl (\pm p -> M.insert p (M.findWithDefault 0 p pm + 1) pm) points (lineToPoints line)

-- | solution 1 is a special case of solution 2, filtered to non diagonal lines
solve1 :: [Line] -> Int
solve1 = solve2 . filter (not . diag)

-- | Add all the points on all the lines to a point frequency map, find points
-- on more than one line, and count them
solve2 :: [Line] -> Int
solve2 = length . filter (>= 2) . M.elems . foldl addPointsToFreqMap M.empty

day05a :: [Line] :~> Int
day05a = MkSol { sParse = parse, sShow = show, sSolve = Just . solve1 }

day05b :: _ :~> _
day05b = MkSol { sParse = parse, sShow = show, sSolve = Just . solve2 }
