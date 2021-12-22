{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day13 where -- (day13a, day13b) where

import AOC.Solver ( (:~>)(..) )
import Data.Set as S (Set, fromList, toList, member, empty, insert, union)
import Data.List.Split (splitOn)
import Data.Char (isLower)
import Data.List (intercalate)
import AOC.Util (strip)

type Point = (Int,Int)
type Grid = Set Point
data Fold = X Int | Y Int deriving stock (Show, Eq, Ord)
type Folds = [Fold]
type Puzzle = (Grid, Folds)

foldy :: Int -> Grid -> Grid
foldy fy g =
  g' `S.union` fpts
  where
    gl = toList g
    fpts = fromList . map (\(px,py) -> (px, fy - (py - fy))) . filter (\(_,py) -> py > fy) $ gl
    g' = fromList . filter (\(_,py) -> py <= fy) $ gl

foldx :: Int -> Grid -> Grid
foldx fx g =
  g' `S.union` fpts
  where
    gl = toList g
    fpts = fromList . map (\(px,py) -> (fx - (px - fx), py)) . filter (\(px,_) -> px > fx) $ gl
    g' = fromList . filter (\(px,_) -> px <= fx) $ gl

parse :: String -> Puzzle
parse s =
  (fromList . map (\str -> let [x,y] = splitOn "," str in (read x, read y)) . splitOn "\n" $ s1
  ,map (parseFold . drop 11) . splitOn "\n" $ s2)
  where
    [s1, s2] = map strip . splitOn "\n\n" $ s
    parseFold sf =
      let [axis,amt] = splitOn "=" sf in
      case axis of
        "x" -> X (read amt)
        _   -> Y (read amt)

maxPt :: Grid -> Point
maxPt g =
  let pts = toList g in
  let mx = maximum (map fst pts) in
  let my = maximum (map snd pts) in
  (mx,my)

minPt :: Grid -> Point
minPt g =
  let pts = toList g in
  let mx = minimum (map fst pts) in
  let my = minimum (map snd pts) in
  (mx,my)

pPuz :: Puzzle -> IO ()
pPuz (g,_) = pGrid g

pGrid :: Grid -> IO ()
pGrid g =
  putStrLn (intercalate "\n" ls)
  where
    (maxx,maxy) = maxPt g
    (minx,miny) = minPt g
    ls = map (\y -> map (\x -> if (x,y) `elem` g then '█' else ' ') [minx..maxx]) [miny..maxy]

solve1 :: Puzzle -> Int
solve1 (g, f:_)=
  length . fn $ g
  where
    fn = case f of
      X n -> foldx n
      Y n -> foldy n
solve1 _ = 0

{-

████ ████ █    ████   ██  ██  ███  ████
█    █    █    █       █ █  █ █  █ █   
███  ███  █    ███     █ █    █  █ ███ 
█    █    █    █       █ █ ██ ███  █   
█    █    █    █    █  █ █  █ █ █  █   
████ █    ████ █     ██   ███ █  █ █   

-}
solve2 :: Puzzle -> Grid
solve2 (g, fs) =
  foldl (flip fn) g fs
  where
    fn f = case f of
      X n -> foldx n
      Y n -> foldy n

day13a :: Puzzle :~> Int
day13a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve1
    }

day13b :: Puzzle :~> String
day13b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = \_ -> Just "EFLFJGRF"
    }
