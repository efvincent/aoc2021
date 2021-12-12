{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AOC.Challenge.Day09 where -- (day09a, day09b) where

import AOC.Solver ( (:~>)(..) )
import Data.IntMap as IM (IntMap, (!), fromList, empty, insert, fromList, delete, toList, size)
import Data.List.Split (splitOn)
import AOC.Util (strip)

type Grid = IM.IntMap (IM.IntMap Int)

data Puzzle = Puzzle
  { _grid :: Grid
  , _maxx :: Int
  , _maxy :: Int
  } deriving stock (Show)

s = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

mkIdx :: [a] -> IntMap a
mkIdx = IM.fromList . ([0..] `zip`)

parse :: String -> Puzzle
parse s =
  Puzzle g (IM.size (g!0) - 1) (IM.size g - 1)
  where
    g :: Grid = mkIdx . map (mkIdx . map (read . (:[]))) . lines $ s

get :: Grid -> (Int,Int) -> Int
get g (x,y) = g!y!x

solve1 :: Puzzle -> Int
solve1 Puzzle{_grid=g, _maxx=mx, _maxy=my} =
  sum . map ((+ 1) . get g) . filter predicate $ idxs
  where
    idxs = [(x,y) | x <- [0..mx], y <- [0..my] ]
    predicate (x,y) =
      let v = get g (x,y) in
      let w = x <= 0  || get g (x-1,y) > v in
      let e = x >= mx || get g (x+1,y) > v in
      let s = y >= my || get g (x,y+1) > v in
      let n = y <= 0  || get g (x,y-1) > v in
      n && s && e && w

day09a :: Puzzle :~> Int
day09a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve1
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
