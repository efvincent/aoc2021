{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module AOC.Challenge.Day14 where -- (day14a, day14b) where

import AOC.Solver ( (:~>)(..) ) 
import qualified Data.Map as M (Map, (!), (!?), empty, findWithDefault, insert, fromList, map, toList)
import Data.List.Split (splitOn)
import AOC.Util (strip, traceShowIdMsg)
import AOC.Prelude (traceShowId)
import Data.List (sort)
import Data.Map (toList)

type Pair = (Char,Char)
type Rule = (Pair, (Pair, Pair))
type Rules = M.Map Pair (Pair, Pair)
type PairCounts = M.Map Pair (Int, Int)    -- fst -> this step, snd -> next step
type Puzzle = (Rules, PairCounts)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

parse :: String -> Puzzle
parse s =
  let ps = pairs seed in 
  let pcs = foldr addCount M.empty ps in 
  (M.fromList rules, pcs)
  where
    [seed,rawRules] = map strip . splitOn "\n\n" $ s
    mkRule r =
      let [[rl,rr],[c]] = map strip . splitOn "->" $ r in
      ((rl,rr), ((rl,c),(c,rr)))
    rules = map mkRule . lines $ rawRules
    addCount :: Pair -> PairCounts -> PairCounts
    addCount pair pairCounts =
      case pairCounts M.!? pair of
        Just (this,next) -> M.insert pair (this+1,next) pairCounts
        Nothing -> M.insert pair (0, 1) pairCounts

getCounts :: Puzzle -> [(Char,Int)]
getCounts (_,pairCountMap) = 
  let pairCounts = M.toList pairCountMap in
  let charCounts = filter (\(_,c) -> c > 0) . concatMap (\((l,r),(c,_)) -> [(l,c),(r,c)]) $ pairCounts in
  M.toList 
  . foldr (
      \(c,n) acc -> 
        let cur = M.findWithDefault 0 c acc in 
        M.insert c (cur+n) acc) 
      M.empty $ charCounts

run :: Int -> Puzzle -> Puzzle
run c puz = 
  foldr (\_ puzzle -> step puzzle) puz [1..c]

step :: Puzzle -> Puzzle
step (rules, pairCounts) =
  (rules, go (M.toList pairCounts) pairCounts)
  where
    go :: [(Pair, (Int, Int))] -> PairCounts -> PairCounts     
    go ((curPair,(this,next)):pcs) pc | this > 0 =       
      -- this pair is used this turn
      -- look up the two related pairs from the rules
      -- increment both of their "next" turns to += the "this" count
      -- then increment this pair's current count to zero
      let (l,r) = rules M.! curPair in 
      let (_,lnext) = M.findWithDefault (0,0) l pc in 
      let (_,rnext) = M.findWithDefault (0,0) r pc in
      let pc' =
            M.insert l (0,lnext + this) .
            M.insert r (0,rnext + this) $ pc in
      let (_,next') = pc M.! curPair in
      let pc'' = M.insert curPair (0,next') pc' in
      go pcs pc'
    go (_:pcs) pc = go pcs pc
    go [] pc = M.map (\(this,next) -> (next,this)) pc 
      
hist :: String -> M.Map Char Int
hist = go M.empty
  where
    go h [] = h
    go h (c:cs) = 
      case h M.!? c of
        Just n -> go (M.insert c (succ n) h) cs
        Nothing -> go (M.insert c 1 h) cs

-- run :: Int -> Puzzle -> String
-- run steps (rules,seed) = 
--   foldr go seed [1..steps]
--   where
--     go _ s =
--       step (rules, s)

day14a :: _ :~> _
day14a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day14b :: _ :~> _
day14b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
