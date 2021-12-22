{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day14old where -- (day14a, day14b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, (!), (!?), empty, findWithDefault, insert)
import Data.List.Split (splitOn)
import AOC.Util (strip, traceShowIdMsg)
import AOC.Prelude (traceShowId)
import Data.List (sort)
import Data.Map (toList)

type Rule = ([Char], Char)
type Rules = [Rule]
type Puzzle = (Rules, String)

parse :: String -> Puzzle
parse s =
  (rules, s1)
  where
    [s1,s2] = map strip . splitOn "\n\n" $ s
    mkRule r =
      let [r1,r2] = map strip . splitOn "->" $ r in
      (r1, head r2)
    rules = map mkRule . lines $ s2

getNext :: Rules -> [Char] -> Char
getNext (([rc1,rc2],ic):_) [c1,c2] | rc1 == c1 && rc2 == c2 = ic
getNext (_:rest) p = getNext rest p
getNext [] [_,c2] = c2
getNext _ _ = error "invalid input"

step :: Puzzle -> String
step (rules,seed) =
  go seed []
  where
    go (c1:c2:rest) acc  =
      let next = getNext rules [c1,c2] in
      go (c2:rest) (next:c1:acc)
    go [c] acc = reverse $ c:acc
    go _ _ = error "invalid input"

hist :: String -> Map Char Int
hist = go M.empty
  where
    go h [] = h
    go h (c:cs) = 
      case h M.!? c of
        Just n -> go (insert c (succ n) h) cs
        Nothing -> go (insert c 1 h) cs

run :: Int -> Puzzle -> String
run steps (rules,seed) = 
  foldr go seed [1..steps]
  where
    go _ s =
      step (rules, s)

solve1 :: Int -> Puzzle -> Int
solve1 steps p = 
  let counts = sort . map snd . toList . hist . run steps $ p in
  let most = last counts in 
  let least = head counts in 
  (most - least)

solve2 :: Puzzle -> Int
solve2 p = 0

day14x :: Show a => (Puzzle -> a) -> Puzzle :~> a
day14x fn = MkSol { sParse = Just . parse, sShow = show, sSolve = Just . fn }

day14a :: Puzzle :~> Int
day14a = day14x (solve1 10)

day14b :: Puzzle :~> Int
day14b = day14x solve2
