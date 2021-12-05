{-# LANGUAGE TupleSections #-}

module AOC.Challenge.Day04
  (
    day04a
  , day04b
  ) 
  where

import AOC.Solver ( (:~>)(..) )
import Data.List.Split (splitOn)
import Data.List (transpose)

type BoardLine = [(Int,Bool)]
type Board = [BoardLine]
type Winner = (Int, Board)

data Puzzle = Puzzle {
  plays :: [Int]
, boards :: [Board]
} deriving stock Show

parse :: String -> Puzzle
parse raw =
  Puzzle ps (getBoards [] boardsRaw)
  where
    ls = lines raw
    ps = map read . splitOn "," $ head ls :: [Int]
    boardsRaw = drop 2 ls
    getBoards acc [] = acc
    getBoards acc rawLines =
      let rawBoard = take 5 rawLines in
      let l = length rawLines in
      let b :: Board = map (map ((,False) . read) . words) rawBoard in
      if l > 5 then getBoards (b:acc) (drop 6 rawLines) else b:acc

-- | Mark any matches in a board line
markLine :: Int -> BoardLine  -> BoardLine
markLine n = foldr (\(x,m) acc -> if x == n then (x,True):acc else (x,m):acc) []

-- | Mark any matches in a board
markBoard :: Int -> Board -> Board
markBoard n = map (markLine n)

-- | Return true if a line is a winner, meaning all the numbers on
-- the line have been called
lineWinner :: BoardLine -> Bool
lineWinner [] = True
lineWinner ((_,b):rest) = b && lineWinner rest

-- | Return true if a column is a winner, maning all the numbers on
-- the column have been called
colWinner :: Board -> Bool
colWinner b =
  let b' = transpose b in
  foldr (\l w -> w || lineWinner l) False b'

-- | Return true if the board is a winner meaning there's a winning line
-- or column on the board
winner :: Board -> Bool
winner b =
  foldr (\l w -> w || lineWinner l) False b
  || colWinner b

-- | return all the boards that are winners, with the last winner at the head
-- of the list
getWinner :: [Board] -> ([Board], [Board])
getWinner = go [] []
  where
    go checked winners (b:rest) =
      if winner b
      then go checked (b:winners) rest
      else go (b:checked) winners rest
    go checked winners [] = (checked, winners)

-- | Sum of all non marked numbers on the board
unmarkedSum :: Board -> Int
unmarkedSum = foldr ((+) . sumRow) 0
  where
    sumRow :: BoardLine -> Int
    sumRow = foldr (\(n,marked) acc -> if not marked then n+acc else acc) 0

solve1 :: Puzzle -> Maybe Int
solve1 puzzle =
  case go (plays puzzle) (boards puzzle) of
    (_, Just (play, board)) -> Just $ unmarkedSum board * play
    (_, Nothing)            -> Nothing
  where
    go :: [Int] -> [Board] -> ([Board], Maybe Winner)
    go (p:ps) boards =
      let markedBoards = map (markBoard p) boards in
      case getWinner markedBoards of
        (_, b:_) -> (markedBoards, Just (p, b))
        (_, [])  -> go ps markedBoards
    go [] bs = (bs, Nothing)

solve2 :: Puzzle -> Maybe Int
solve2 puzzle =
  case go [] (plays puzzle) (boards puzzle) of
    (_, [])                   -> Nothing
    (_, (play, lastWinner):_) -> Just $ unmarkedSum lastWinner * play
  where
    go :: [Winner] -> [Int] -> [Board] -> ([Board], [Winner])
    go winners []               losers     = (losers, winners) -- no more numbers to play
    go winners _                []         = ([]    , winners) -- no non-winning boards left
    go winners (play:remaining) nonWinners =                   -- plays and non-winning boards remain
      let boards = map (markBoard play) nonWinners in
      case getWinner boards of
        (boards', newWinners@(_:_)) -> go (map (play,) newWinners ++ winners) remaining boards'
        (boards', [])               -> go winners remaining boards'

day04a :: Puzzle :~> Int
day04a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = solve1
    }

day04b :: Puzzle :~> Int
day04b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = solve2
    }