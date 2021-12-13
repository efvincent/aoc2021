{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AOC.Challenge.Day09 where -- (day09a, day09b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, (!), fromList, empty, insert, fromList, delete, toList, size, keys)
import Data.List.Split (splitOn)
import AOC.Util (strip)
import AOC.Prelude (traceShowMsg, traceShow, traceShowId)

data Point = Point
  { _x :: Int
  , _y :: Int }
  deriving stock (Show, Ord, Eq)

data LocState
  = Unknown
  | Searching
  | Low
  | Basin Point
  deriving stock (Show)

data Location = Location
  { _pos :: Point
  , _state :: LocState
  , _value :: Int }
  deriving stock (Show)

instance Ord Location where
  compare l1 l2 = compare (_pos l1) (_pos l2)

instance Eq Location where
  (==) l1 l2 = _pos l1 == _pos l2

type Grid = M.Map Point Location

data Puzzle = Puzzle
  { _grid :: Grid
  , _maxx :: Int
  , _maxy :: Int
  } deriving stock (Show)

s :: [Char]
s = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

mkIdx :: [a] -> Map Int a
mkIdx = M.fromList . ([0..] `zip`)

solve2 :: Puzzle -> Int
solve2 p@Puzzle{_grid=g, _maxx=mx, _maxy=my} =
  0
  where
    q = 0

solve1 :: Puzzle -> Int
solve1 Puzzle{_grid=g, _maxx=mx, _maxy=my} =
  sum . map ((+ 1) . _value . get g) . filter predicate $ idxs
  where    
    idxs = [Point x y | x <- [0..mx], y <- [0..my] ]
    predicate :: Point -> Bool
    predicate p =
      let gval = _value . get g in
      let (x,y) = (_x p, _y p) in
      let v = gval p in
      let w = x <= 0  || gval (Point (x-1) y) > v in
      let e = x >= mx || gval (Point (x+1) y) > v in
      let s = y >= my || gval (Point x (y+1)) > v in
      let n = y <= 0  || gval (Point x (y-1)) > v in
      n && s && e && w

parse :: String -> Puzzle
parse s =
  let mp = maximum . M.keys $ g in
  let (mx,my) = (_x mp, _y mp) in
  Puzzle g mx my
  where
    g = M.fromList
        . map (
          \(x,(v,y)) ->
            let p = Point x y in
            (p, Location p Unknown v))
        . concatMap (
          \(y,l) ->
            [0..] `zip`
            map ((,y) . subtract 48 . fromEnum) l
        )
        . ([0..] `zip`)
        . lines $ s


    -- g :: Grid = mkIdx . map (mkIdx . map (\c -> Location Unknown (fromEnum c - 48))) . lines $ s

get :: Grid -> Point -> Location
get p coord =  (! coord) p

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
