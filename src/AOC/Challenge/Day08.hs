{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day08 where -- (day08a, day08b) where

import AOC.Solver ( (:~>)(..) )
import Data.IntMap as IM (IntMap, (!), empty, insert, fromList, elems, delete, toList)
import Data.Map as M ((!), fromList)
import Data.Set as S (Set, fromList, member, intersection)
import Data.List.Split (splitOn)
import AOC.Util (strip)
import Data.Tuple (swap)
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

type Digit = S.Set Char

type DigitMap = IM.IntMap Digit

type Puzzles = [Puzzle]

type Decoder = Int -> DState -> DState

data DState = DState
  { _unk :: DigitMap
  , _known :: DigitMap
  } deriving stock (Show)

data Puzzle = Puzzle
  { _state :: DState
  , _input :: [Digit]
  } deriving stock (Show)

-- | totally not needed use of intersection symbol
(⋂) :: Ord a => Set a -> Set a -> Set a
(⋂) = S.intersection

-- | parse one line consisting of 10 observations and an encoded 4 digit number
parseLine :: String -> Puzzle
parseLine s =
  let [obs,ins] = map (map S.fromList . splitOn " " . strip) . splitOn "|" $ s in
  Puzzle { 
    _state = DState { 
      _unk = IM.fromList . zip [0..] $ obs, 
      _known = IM.empty }, 
    _input = ins }

-- | Solves part 1 by finding the number of encoded digits across all the puzzle
-- lines that are uniquely identified by the number of segments in the digit
solveLine1 :: Puzzle -> Int
solveLine1 p =
  let targets = S.fromList [2,3,4,7] in
  length . filter (`S.member` targets) . map length . _input $ p

-- | Identifies the @idSought@ digit by finding the unkown digit with the specified
-- length. This is a partial function in that it assumes that what you're asking
-- for exists in the unknowns
findByLen :: Int -> Decoder
findByLen lenSought idSought d@DState{_unk=unk, _known=known} =
  go (IM.toList unk)
  where
    go ((unkId, unkBits):_)
      | length unkBits == lenSought =
        d { _unk=IM.delete unkId unk , _known=IM.insert idSought unkBits known }
    go (_:rest) = go rest
    go [] = d

-- | Identifies the @idSought@ digit by testing that each pair in @specs@ 
-- tests true. For one of these pairs to test true, the @fst@ indexes a known 
-- digit, and the @snd@ is the expected length of the ⋂ of the candidate 
-- unknown digit and the indexed known digit. Importantly, when an unknown 
-- digit is found, it's removed from the unknown digit map as well as
-- being added to the known digit map in the current decoder state
findBySpec :: [(Int, Int)] -> Decoder
findBySpec specs idSought d@DState{_unk=unk, _known=known} =
  go specs (IM.toList unk)
  where
    test :: Digit -> [(Int, Int)] -> Bool
    test unkBits ((compId, iLen):rest)
      | length (unkBits ⋂ (known IM.! compId)) == iLen = 
        test unkBits rest
    test _ (_:_) = False
    test _ [] = True

    go :: [(Int, Int)] -> [(Int, Digit)] -> DState
    go comps ((unkId, unkBits):_) 
      | test unkBits comps =
        d { _unk=IM.delete unkId unk, _known=IM.insert idSought unkBits known }
    go comps (_:rest) = go comps rest
    go _ [] = d

-- | Finds the unknown digits by first identifying the unknown digits that can be
-- identified by the number of segments, then finds them using the known relationships
-- between remaining unknown digits and the ever growning number of known digits
decode :: Puzzle -> DState
decode =
  findBySpec [(6,5)] 0 .
  findBySpec [(6,4)] 3 .
  findBySpec [(1,1),(7,2),(2,4)] 6 .
  findBySpec [(1,1),(7,2),(2,3)] 5 .
  findBySpec [(1,2),(7,3),(4,4)] 9 .
  findBySpec [(1,1),(7,2),(4,2)] 2 .
  findByLen 7 8 .
  findByLen 3 7 .
  findByLen 4 4 .
  findByLen 2 1 . _state

-- | solves part 2 by fully decoding the 4 digits in the input, and interpreting them
-- as an integer
solveLine2 :: Puzzle -> Int
solveLine2 p@Puzzle{ _input=inp } =
  sum . zipWith (*) [1000,100,10,1] $ digits 
  where
    known = M.fromList . map swap . IM.toList . _known . decode $ p
    digits = map (known M.!) inp

-- | uses a line solver @(Puzzle -> Int)@ to solve all the puzzles, summing up each
-- puzzle's solution to give the overall solution
solve :: (Puzzle -> Int) -> Puzzles -> Int
solve fn = sum . map fn

day08a :: Puzzles :~> Int
day08a = MkSol
  { sParse = Just . map parseLine . lines
  , sShow  = show
  , sSolve = Just . solve solveLine1
  }

day08b :: Puzzles :~> Int
day08b = MkSol
  { sParse = Just . map parseLine . lines
  , sShow  = show
  , sSolve = Just . solve solveLine2
  }
