{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day13 (day13a, pGrid13, solve2d13) where

import AOC.Solver ( (:~>)(..) )
import Data.Set as S (Set, fromList, toList, union)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import AOC.Util (strip)

type Point = (Int,Int)
type Grid = Set Point
data Fold = X Int | Y Int deriving stock (Show, Eq, Ord)
type Folds = [Fold]
type Puzzle = (Grid, Folds)

foldy :: Int -> Grid -> Grid
foldy fy g =
  trimmed `S.union` folded
  where
    gl = toList g
    folded = fromList . map (\(px,py) -> (px, fy - (py - fy))) . filter (\(_,py) -> py > fy) $ gl
    trimmed = fromList . filter (\(_,py) -> py <= fy) $ gl

foldx :: Int -> Grid -> Grid
foldx fx g =
  trimmed `S.union` folded
  where
    gl = toList g
    folded = fromList . map (\(px,py) -> (fx - (px - fx), py)) . filter (\(px,_) -> px > fx) $ gl
    trimmed = fromList . filter (\(px,_) -> px <= fx) $ gl

parse :: String -> Puzzle
parse s =
  ( fromList . map (\str -> let [x,y] = splitOn "," str in (read x, read y)) . splitOn "\n" $ s1
  , map (parseFold . drop 11) . splitOn "\n" $ s2)
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

pGrid13 :: Grid -> IO ()
pGrid13 g =
  putStrLn (intercalate "\n" ls)
  where
    (maxx,maxy) = maxPt g
    ls = map (\y -> map (\x -> if (x,y) `elem` g then toEnum 9608 else ' ') [0..maxx]) [0..maxy]

solve1 :: Puzzle -> Int
solve1 (g, f:_)=
  length . fn $ g
  where
    fn = case f of
      X n -> foldx n
      Y n -> foldy n
solve1 _ = 0

{-
Solution ... was not going to do character recognition just
to be able to submit the solution via API

████ ████ █    ████   ██  ██  ███  ████
█    █    █    █       █ █  █ █  █ █   
███  ███  █    ███     █ █    █  █ ███ 
█    █    █    █       █ █ ██ ███  █   
█    █    █    █    █  █ █  █ █ █  █   
████ █    ████ █     ██   ███ █  █ █   

-}

solve2d13 :: String -> Grid
solve2d13 s =
  let (g, fs) = parse s in
  foldl (flip fn) g fs
  where
    fn f = case f of
      X n -> foldx n
      Y n -> foldy n

day13a :: Puzzle :~> Int
day13a = MkSol { sParse = Just . parse, sShow  = show, sSolve = Just . solve1 }
