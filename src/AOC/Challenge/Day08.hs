{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day08 where -- (day08a, day08b) where

import AOC.Solver ( (:~>)(..) )
import Data.List (sort)
import Data.IntMap as IM (IntMap, (!), empty, insert, lookup)
import Data.Set as S (Set, empty, fromList, member, intersection)
import Data.List.Split (splitOn)
import AOC.Util (strip)
import Control.Monad ( foldM )

s :: String
s = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

{-
algo

step 1 - search by length of set to find 1 7 4 8
  |x| ≡ 2 ⊢ x ≡ d1        
  |x| ≡ 3 ⊢ x ≡ d7
  |x| ≡ 4 ⊢ x ≡ d4
  |x| ≡ 7 ⊢ x ≡ d8
  
  known: |d2 d7 d4 d8|

step 2 - find by length of intersection with known through step 1
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 2 ⊢ x ≡ d2
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 4 ⊢ x ≡ d9

  known: |d1 d2 d4 d7 d8 d9|

step 3 - find by length of intersection with known through step 2
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d2| ≡ 3 ⊢ x ≡ d5
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d2| ≡ 4 ⊢ x ≡ d6

  known: |d1 d2 d4 d5 d6 d7 d8 d9|
  
step 4 - find by length of intersection with known through step 3
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d6| ≡ 4 ⊢ x ≡ d3
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d6| ≡ 5 ⊢ x ≡ d0

  known: |d1 d2 d3 d4 d5 d6 d7 d8 d9 d0|

The order of evaluation should be according to these steps. For each candidate as we
determine what d# it is, that set of "wires" should be added to map of d# to Set(wires)
so it will be available for the next test
-}

-- tests :: [Int]
-- tests = [1,7,4,8,2,9,5,6,3,0]

type Digit = (S.Set Char)
type Rosetta = IM.IntMap Digit
type Samples = [Digit]
type Test = Digit -> Rosetta -> Maybe (Int, Digit)
type PuzzleLine = ([Digit], [Digit])
type Puzzle = [PuzzleLine]

parse :: String -> Puzzle
parse =
  let toDigit = map S.fromList . splitOn " " in
  map (((\[k,o] -> (k,o)) . map toDigit) . (map strip . splitOn "|")) . lines

solve1 :: Puzzle -> Int
solve1 =
  let targets = S.fromList [2,3,4,7] in
  length . filter (`S.member` targets) . concatMap (map length . snd)

checkByLen :: Int -> Int -> Rosetta -> Digit -> Rosetta
checkByLen seek num r digit = if length digit == seek then IM.insert num digit r else r

checkByInter :: Int -> Int -> Rosetta -> Digit -> Rosetta
checkByInter seek num r digit = 
  let comp = r IM.! seek in
  if length (digit `S.intersection` comp) == seek
  then IM.insert num digit r
  else r

findDigit :: Int -> Rosetta -> Digit -> Rosetta
findDigit 1 r = checkByLen 2 1 r
findDigit 7 r = checkByLen 3 7 r
findDigit 4 r = checkByLen 4 4 r
findDigit 8 r = checkByLen 7 8 r

day08a :: Puzzle :~> Int
day08a = MkSol
  { sParse = Just . parse
  , sShow  = show
  , sSolve = Just . solve1
  }

day08b :: _ :~> _
day08b = MkSol
  { sParse = Just
  , sShow  = show
  , sSolve = Just
  }
