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

-- | Returns the costs for a token type - first is the error score, second
-- is the incomplete score for that token type    
scoreOf :: BType -> (Int,Int)
scoreOf = \case 
  Round  -> (3,1)
  Square -> (57,2)
  Curly  -> (1197,3)
  Angle  -> (25137,4)

-- | Walk the line, handling the different cases. When there's no more input, and
-- nothing on the stack, then the line has no error. When there's no stack and the 
-- next input is a close, that's an error. When there's an open followed by a close
-- of a different token type, that's an error. When there's an open followed by a close,
-- pop the open off the stack, discard the close, and loop with remaining stack and
-- input. Finally when the next token is an open, push it on the stack and continue
errScore :: Line -> Int
errScore = go []
  where
    go _ []                                    = 0
    go [] ((Close ct):_)                       = fst $ scoreOf ct
    go ((Open ot):_) ((Close ct):_) | ct /= ot = fst $ scoreOf ct
    go ((Open _):tkns) ((Close _):tkns')       = go tkns tkns'
    go stack (open:tkns')                      = go (open:stack) tkns'

-- | If a line has an error, it cannot be checked for incompleteness. Otherwise, 
-- when there's no more input, incomplete score is determined by remaining opens
-- on the stack. When there's an open on the stack and a close in the input, pop the 
-- open off the stack, discard the close, and continue. When there's an open in 
-- the input, push it on the stack
incompleteScore :: Line -> Int
incompleteScore ln =
  case errScore ln of
    0 -> foldl (\acc n -> (acc * 5) + n) 0 (go [] ln)
    _ -> 0 
  where
    go stack []                          = map (\(Open x) -> snd . scoreOf $ x) stack
    go ((Open _):tkns) ((Close _):tkns') = go tkns tkns'
    go stack (open:tkns)                 = go (open:stack) tkns

-- | Find the incompete scores for each line and return the median
solve2 :: [Line] -> Int
solve2 lns =
  let scores = sort . filter (/= 0) . map incompleteScore $ lns in
  (scores !! (length scores `div` 2))    

day10x :: Show a => ([Line] -> a) -> [Line] :~> a
day10x fn = MkSol { sParse = Just . parse, sShow = show, sSolve = Just . fn}

-- | solution for part 1 is just the sum of the error scores for each line
day10a :: [Line] :~> Int
day10a = day10x (sum . map errScore)

day10b :: [Line] :~> Int
day10b = day10x solve2
