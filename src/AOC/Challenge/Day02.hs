{-# LANGUAGE LambdaCase #-}

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import AOC.Prelude ( (:~>)(..) )
import Data.List.Split (splitOn)

type Instr = (Char,Int)
type Pos   = (Int ,Int)

parse :: String -> [Instr]
parse s = go [] (map (splitOn " ") . lines $ s)
  where
    go ins [] = reverse ins
    go ins ([dir, v]:rest) = go ((head dir, read v):ins) rest
    go _ _ = error "Bad input"

solve1 :: Pos -> [Instr] -> Int
solve1 (h,d) = \case 
  ('f', n):rest -> solve1 (h + n, d)     rest
  ('d', n):rest -> solve1 (h    , d + n) rest
  ('u', n):rest -> solve1 (h    , d - n) rest
  _             -> h * d

solve2 :: Int -> Pos -> [Instr] -> Int
solve2 a (h,d) = \case
  ('f', n):rest -> solve2 a       (h + n, d + (a * n)) rest
  ('d', n):rest -> solve2 (a + n) (h    , d)           rest
  ('u', n):rest -> solve2 (a - n) (h    , d)           rest
  _             -> h * d

day02a :: [Instr] :~> Int
day02a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve1 (0,0)
    }

day02b :: [Instr] :~> Int
day02b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve2 0 (0,0)
    }