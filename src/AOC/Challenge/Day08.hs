{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day08 where -- (day08a, day08b) where

import AOC.Solver ( (:~>)(..) )
import Data.List (sort)
import Data.IntMap as IM (IntMap, (!), empty, insert, lookup, fromList, elems, delete, toList)
import Data.Map as M (Map, (!), empty, insert, toList, fromList)
import Data.Set as S (Set, empty, fromList, member, intersection, toList)
import Data.List.Split (splitOn)
import AOC.Util (strip)
import Control.Monad ( foldM )
import Data.Tuple (swap)

sample :: String
sample = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

sample2 :: [Char]
sample2 = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
{-
algo

unk is the set of 10 unknown observations

step 1 - search by length of set to find 1 7 4 8
  known: {}
  unknown: {u_i | i <- [0..9]}  

  |x| ≡ 2 ⊢ x ≡ d1        
  |x| ≡ 3 ⊢ x ≡ d7
  |x| ≡ 4 ⊢ x ≡ d4
  |x| ≡ 7 ⊢ x ≡ d8
  
    known: {d2, d4, d7, d8}
  unknown: {u_i | i <- [0..5] }  

step 2 - find by length of intersection with known through step 1
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 2 ⊢ x ≡ d2
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 4 ⊢ x ≡ d9

    known: {d1, d2, d4, d7, d8, d9}
  unknown: {u_i | i <- [0..3]}  

step 3 - find by length of intersection with known through step 2
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d2| ≡ 3 ⊢ x ≡ d5
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d2| ≡ 4 ⊢ x ≡ d6

    known: {d1, d2, d4, d5, d6, d7, d8, d9}
  unknown: {u_i | i <- [0..1]}  

step 4 - find by length of intersection with known through step 3
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d6| ≡ 4 ⊢ x ≡ d3
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d6| ≡ 5 ⊢ x ≡ d0

    known: {d1, d2, d3, d4, d5, d6, d7, d8, d9, d0}
  unknown: {}  

The order of evaluation should be according to these steps. For each candidate as we
determine what d# it is, that set of "wires" should be added to map of d# to Set(wires)
so it will be available for the next test
-}

-- tests :: [Int]
-- tests = [1,7,4,8,2,9,5,6,3,0]

type Digit = S.Set Char

type DigitMap = IM.IntMap Digit

type Puzzles = [Puzzle]

type Decoder = Int -> DecoderState -> DecoderState

data DecoderState = DecoderState
  { _unk :: DigitMap
  , _known :: DigitMap
  } deriving stock (Show)

data Puzzle = Puzzle
  { _state :: DecoderState
  , _input :: DigitMap
  } deriving stock (Show)

decodeByLen :: Int -> Decoder
decodeByLen lenSought idSought d@DecoderState{_unk=unk, _known=known} =
  go (IM.toList unk)
  where
    go ((unkId, unkBits):_) 
      | length unkBits == lenSought = 
        d { _unk=IM.delete unkId unk , _known=IM.insert idSought unkBits known }
    go (_:rest) = go rest
    go [] = d

decodeByComp :: [(Int, Int)] -> Decoder
decodeByComp comparisons idSought d@DecoderState{_unk=unk, _known=known} =
  go comparisons (IM.toList unk)
  where
    match :: Digit -> Digit -> Int -> Bool
    match s1 s2 l = length (s1 `S.intersection` s2) == l

    test :: Digit -> [(Int, Int)] -> Bool
    test unkBits ((compId, iLen):rest) 
      | match unkBits (known IM.! compId) iLen = test unkBits rest
    test _ (_:_) = False
    test _ [] = True

    go :: [(Int, Int)] -> [(Int, Digit)] -> DecoderState
    go comps ((unkId, unkBits):_) | test unkBits comps = 
      d { _unk=IM.delete unkId unk, _known=IM.insert idSought unkBits known }
    go comps (_:rest) = go comps rest
    go _ [] = d 

decode :: Puzzle -> DecoderState
decode =
  decodeByComp [(6,5)] 0 .
  decodeByComp [(6,4)] 3 .
  decodeByComp [(1,1),(7,2),(2,4)] 6 .
  decodeByComp [(1,1),(7,2),(2,3)] 5 .
  decodeByComp [(1,2),(7,3),(4,4)] 9 .
  decodeByComp [(1,1),(7,2),(4,2)] 2 .
  decodeByLen 7 8 .
  decodeByLen 3 7 .
  decodeByLen 4 4 .
  decodeByLen 2 1 . _state

solveLinePart2 :: Puzzle -> Int
solveLinePart2 p =
  foldr (\(x,y) acc -> (x * y) + acc) 0 npairs
  where
    known = M.fromList . map swap . IM.toList . _known . decode $ p
    inp =   map snd . IM.toList . _input $ p
    digits :: [Int] = map (known M.!) inp
    npairs = digits `zip` ([1000,100,10,1]::[Int])

parseLine :: String -> Puzzle
parseLine s =
  let [obs,ins] = map toDigits . splitOn "|" $ s in
  Puzzle { _state = DecoderState { _unk = obs, _known = IM.empty }, _input = ins}
  where
    toDigits :: String -> DigitMap = IM.fromList . zip [0..] . map S.fromList . splitOn " " . strip

parse :: String -> [Puzzle]
parse = map parseLine . lines

solveLinePart1 :: Puzzle -> Int
solveLinePart1 p =
  let targets = S.fromList [2,3,4,7] in
  length . filter (`S.member` targets) . map length . IM.elems . _input $ p

solve :: (Puzzle -> Int) -> Puzzles -> Int
solve fn = sum . map fn

day08a :: Puzzles :~> Int
day08a = MkSol
  { sParse = Just . parse
  , sShow  = show
  , sSolve = Just . solve solveLinePart1
  }

day08b :: Puzzles :~> Int
day08b = MkSol
  { sParse = Just . parse
  , sShow  = show
  , sSolve = Just . solve solveLinePart2
  }
