{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day14 (day14a, day14b) where

import AOC.Solver ( (:~>)(..) )
import qualified Data.Map as M (Map, (!), (!?), empty, findWithDefault, insert, fromList, map, toList)
import Data.List.Split (splitOn)
import AOC.Util (strip)
import Data.List (sort)

type Pair = (Char,Char)
type Rules = M.Map Pair (Pair, Pair)
type PairCounts = M.Map Pair (Int, Int)    -- fst -> this step, snd -> next step
type CharCounts = M.Map Char Int
type Puzzle = (Rules, PairCounts, CharCounts)

bumpCount :: (Ord k) => Int -> k -> M.Map k Int -> M.Map k Int
bumpCount n k m =
  case m M.!? k of
    Just x  -> M.insert k (n+x) m
    Nothing -> M.insert k n m

parse :: String -> Puzzle
parse s =
  let ps = zip seed (tail seed) in
  let cCounts = foldr (bumpCount 1) M.empty seed in
  let pcs = foldr addCount M.empty ps in
  (M.fromList rules, pcs, cCounts)
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
        Nothing -> M.insert pair (1, 0) pairCounts

step :: Puzzle -> Puzzle
step puz@(_, pairCounts, _) =
  let curPairCounts = M.toList pairCounts in
  go curPairCounts puz
  where
    go :: [(Pair, (Int, Int))] -> Puzzle -> Puzzle
    go ((curPair,(curCount,_)):pcs) (rules,pc,cCounts) | curCount > 0 =
      -- identified a pair that has occurences this iteration
      -- look up in the rules the two pairs generated from this pair
      -- increment both of their "next" turns to += the "curCount"
      -- Add curCount instances of this pairs "addedChar" to the cCounts.
      let (leftPair@(_,addedChar),rightPair) = rules M.! curPair in
      let (_,lnext) = M.findWithDefault (0,0) leftPair  pc in
      let (_,rnext) = M.findWithDefault (0,0) rightPair pc in
      let cCounts' = bumpCount curCount addedChar cCounts in
      let pc' =
            M.insert leftPair  (0,lnext + curCount) .
            M.insert rightPair (0,rnext + curCount) $ pc in
      go pcs (rules, pc', cCounts')
    go (_:pcs) pc = go pcs pc
    go [] (rules, finalPairCounts, charCounts) =
      let nextPairCounts = M.map (\(_,next) -> (next,0)) finalPairCounts in
      (rules, nextPairCounts, charCounts)

ans :: Int -> Puzzle -> Int
ans numSteps puz =
  let (_,_,cCounts) = foldr (\_ puzzle -> step puzzle) puz [1..numSteps] in
  let hist = sort . map snd . M.toList $ cCounts in
  last hist - head hist

day14x :: Int -> Puzzle :~> Int 
day14x n = MkSol { sParse = Just . parse, sShow = show, sSolve = Just . ans n }

day14a :: Puzzle :~> Int
day14a = day14x 10

day14b :: _ :~> _
day14b = day14x 40