{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AOC.Challenge.Day11 where -- (day11a, day11b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, (!), fromList, keys, adjust, toList, insert)
import Data.Set as S (Set, empty, insert, toList, member)
import AOC.Prelude (catMaybes)
import Data.List (sortBy, insert)
import Data.Ord (comparing, Down (Down))
import Data.Maybe ( fromMaybe )
import Data.List.Split (chunksOf)

type Point = (Int,Int)
type Grid = M.Map Point Int

data Puzzle = Puzzle
  { _grid :: Grid
  , _maxx :: Int
  , _maxy :: Int }
  deriving stock Show

e = "11111\n19991\n19191\n19991\n11111"

parse :: String -> Puzzle
parse s =
  let (mx,my) = maximum . M.keys $ g in
  Puzzle g mx my
  where
    g = M.fromList
        . map       (\(y,(v,x)) -> ((x,y), v))
        . concatMap (\(y,l)     -> [0..] `zip` map ((,y) . subtract 48 . fromEnum) l)
        . ([0..] `zip`)
        . lines $ s

neighbors :: Puzzle -> Point -> [Point]
neighbors Puzzle{_maxx=mx, _maxy=my} (x,y) =
  [(a,b) | a <- [(x-1)..(x+1)]
         , b <- [(y-1)..(y+1)]
         , (a,b) /= (x,y)
         , a >= 0 && a <= mx
         , b >= 0 && b <= my ]

allPoints :: Puzzle -> [Point]
allPoints Puzzle{_maxx=mx, _maxy=my} =
  [(x,y) | x <- [0..mx], y <- [0..my]]

pPuz :: Puzzle -> IO ()
pPuz Puzzle{_grid=g, _maxx=mx} =
  putStrLn $ unlines . chunksOf (mx+1) . map (toEnum . (+48) . snd) . M.toList $ g  

step :: Puzzle -> (Int,Puzzle)
step puz@Puzzle{_grid=g} =
  let (flashes, grid) = loop S.empty g' in
  (flashes, puz { _grid=grid })
  where
    g' = (+1) <$> g
    allPts = allPoints puz

    flash :: Set Point -> Grid -> Point -> Bool
    flash fs grid p = (grid!p) > 9 && not (S.member p fs)

    zero grid = 
      foldr (\p acc -> 
        let v = acc!p in 
        if v <= 9 then acc else M.insert p 0 acc) grid allPts

    go fs grid [] = (fs,grid)
    go fs grid (p:pts) | flash fs grid p =
      let grid' = foldr (M.adjust (+1)) grid (neighbors puz p) in
      go (S.insert p fs) grid' pts
    go fs grid (_:pts) = go fs grid pts

    loop fs grid =
      let (fs', grid') = go fs grid allPts in
      if length fs' > length fs  
      then loop fs' grid'
      else (length fs, zero grid')

steps :: Int -> Puzzle -> (Int, Puzzle)
steps i p = foldr (\_ (c,puz) -> let (c',puz') = step puz in (c+c', puz')) (0,p) [1..i]

solve1 :: Puzzle -> Int
solve1 = fst . steps 100

solve2 :: Puzzle -> Int
solve2 = go 1
  where
    go n puz =
      let (_,puz') = step puz in
      if any ((> 0) . snd) (M.toList (_grid puz'))
      then go (n+1) puz'
      else n 

day11a :: Puzzle :~> Int
day11a = MkSol
  { sParse = Just . parse
  , sShow  = show
  , sSolve = Just . solve1
  }

day11b :: _ :~> _
day11b = MkSol
  { sParse = Just . parse
  , sShow  = show
  , sSolve = Just . solve2
  }
