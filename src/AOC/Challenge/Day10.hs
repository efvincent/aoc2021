{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day10 (day10a, day10b) where

import AOC.Solver ( (:~>)(..) )
import Data.List (sort)

data BType = Round | Square | Curly | Angle
             deriving stock (Show, Eq)

data Token = Open BType | Close BType
             deriving stock (Show)

type Line = [Token]

parse :: String -> [Line]
parse =
  map (map charToToken) . lines
  where
    charToToken = \case 
      '(' -> Open Round
      '[' -> Open Square
      '{' -> Open Curly
      '<' -> Open Angle
      ')' -> Close Round
      ']' -> Close Square
      '}' -> Close Curly
      '>' -> Close Angle
      c   -> error ("Invalid input character : '" ++ show c ++ "'")
    
scoreOf :: BType -> (Int,Int)
scoreOf = \case 
  Round  -> (3,1)
  Square -> (57,2)
  Curly  -> (1197,3)
  Angle  -> (25137,4)

errScore :: Line -> Int
errScore = go []
  where
    go _ []                                    = 0
    go [] ((Close ct):_)                       = fst $ scoreOf ct
    go ((Open ot):_) ((Close ct):_) | ct /= ot = fst $ scoreOf ct
    go ((Open _):tkns) ((Close _):tkns')       = go tkns tkns'
    go stack (open:tkns')                      = go (open:stack) tkns'

incScore :: Line -> Int
incScore ln =
  case errScore ln of
    0 -> foldl (\acc n -> (acc * 5) + n) 0 (go [] ln)
    _ -> 0 
  where
    go stack []                          = map (\(Open x) -> snd . scoreOf $ x) stack
    go ((Open _):tkns) ((Close _):tkns') = go tkns tkns'
    go stack (open:tkns)                 = go (open:stack) tkns

solve2 :: [Line] -> Int
solve2 lns =
  let scores = sort . filter (/= 0) . map incScore $ lns in
  (scores !! (length scores `div` 2))    

day10x :: Show a => ([Line] -> a) -> [Line] :~> a
day10x fn = MkSol { sParse = Just . parse, sShow = show, sSolve = Just . fn}

day10a :: [Line] :~> Int
day10a = day10x (sum . map errScore)

day10b :: [Line] :~> Int
day10b = day10x solve2
