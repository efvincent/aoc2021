{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AOC.Challenge.Day17 where -- (day17a, day17b) where

import AOC.Solver ( (:~>)(..) )
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches))
import Data.List.Split (splitOn)

type Point = (Int,Int)
type Puzzle = (Point, Point)

parse :: String -> Puzzle
parse s =
  let [sx, sy] = getAllTextMatches (s =~ "(-?[0-9]+)..(-?[0-9]+)") in
  (toPair sx, toPair sy)
  where
    toPair :: String -> Point
    toPair str = let [s1,s2] = map read . splitOn ".." $ str in (s1,s2)

day17a :: _ :~> _
day17a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day17b :: _ :~> _
day17b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
