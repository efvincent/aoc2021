{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day08 (day08a, day08b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, (!), fromList, empty, insert, fromList, delete, toList)
import Data.Set as S (Set, fromList, member, intersection)
import Data.List.Split (splitOn)
import AOC.Util (strip)
import Data.Tuple (swap)

{- algorithm for part2

step 1 - search by length of set to find 1 7 4 8
  known: {}
  |unknown| = 10  

  |x| ≡ 2 ⊢ x ≡ d1        
  |x| ≡ 3 ⊢ x ≡ d7
  |x| ≡ 4 ⊢ x ≡ d4
  |x| ≡ 7 ⊢ x ≡ d8
  
  known: {d2, d4, d7, d8}
  |unknown| = 6  

step 2 - find by length of intersection with known through step 1
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 2 ⊢ x ≡ d2
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 4 ⊢ x ≡ d9

  known: {d1, d2, d4, d7, d8, d9}
  |unknown| = 4  

step 3 - find by length of intersection with known through step 2
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d2| ≡ 3 ⊢ x ≡ d5
  |x ⋂ d1| ≡ 1 ∧ |x ⋂ d7| ≡ 2 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d2| ≡ 4 ⊢ x ≡ d6

  known: {d1, d2, d4, d5, d6, d7, d8, d9}
  |unknown| = 2  

step 4 - find by length of intersection with known through step 3
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d6| ≡ 4 ⊢ x ≡ d3
  |x ⋂ d1| ≡ 2 ∧ |x ⋂ d7| ≡ 3 ∧ |x ⋂ d4| ≡ 3 ∧ |x ⋂ d6| ≡ 5 ⊢ x ≡ d0

  known: {d1, d2, d3, d4, d5, d6, d7, d8, d9, d0}
  |unknown| = 0    
-}

type Digit    = Set Char
type DigitMap = Map Int Digit
type Puzzles  = [Puzzle]

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
(⋂) = intersection

-- | parse one line consisting of 10 observations and an encoded 4 digit number
parseLine :: String -> Puzzle
parseLine s =
  let [obs,ins] = map (map S.fromList . splitOn " " . strip) . splitOn "|" $ s in
  Puzzle {
    _state = DState {
      _unk = M.fromList . zip [0..] $ obs,
      _known = empty },
    _input = ins }

-- | Solves part 1 by finding the number of encoded digits across all the puzzle
-- lines that are uniquely identified by the number of segments in the digit
solveLine1 :: Puzzle -> Int
solveLine1 p =
  let targets = S.fromList [2,3,4,7] in
  length . filter (`member` targets) . map length . _input $ p

-- | Identifies digits by their length. Each tuple in the @[(Int,Int)]@ list
-- is a length and the identity of the digit that is uniquely identified
-- by having that number of segments in the digit
findByLen :: [(Int,Int)] -> DState -> DState
findByLen [] st = st
findByLen ((len,ident):rest) state =
  findByLen rest (test (M.toList . _unk $ state) len ident state)
  where
    test ((unkId, unkBits):_) l i st
      | length unkBits == l =
        st { _unk=delete unkId (_unk st)
           , _known=insert i unkBits (_known st) }
    test (_:tests) l i st  = test tests l i st
    test [] _ _ st = st

-- | Identifies the @idSought@ digit by testing that each pair in @specs@ 
-- tests true. For one of these pairs to test true, the @fst@ indexes a known 
-- digit, and the @snd@ is the expected length of the ⋂ of the candidate 
-- unknown digit and the indexed known digit. Importantly, when an unknown 
-- digit is found, it's removed from the unknown digit map as well as
-- being added to the known digit map in the current decoder state
findBySpec :: [(Int, Int)] -> Int -> DState -> DState
findBySpec specs idSought d@DState{_unk=unk, _known=known} =
  go specs (toList unk)
  where
    test :: Digit -> [(Int, Int)] -> Bool
    test unkBits ((compId, iLen):rest)
      | length (unkBits ⋂ (known ! compId)) == iLen =
        test unkBits rest
    test _ (_:_) = False
    test _ [] = True

    go :: [(Int, Int)] -> [(Int, Digit)] -> DState
    go comps ((unkId, unkBits):_)
      | test unkBits comps =
        d { _unk=delete unkId unk, _known=insert idSought unkBits known }
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
  findByLen  [(2,1),(4,4),(3,7),(7,8)] . _state

-- | solves part 2 by fully decoding the 4 digits in the input, and interpreting them
-- as an integer
solveLine2 :: Puzzle -> Int
solveLine2 p@Puzzle{ _input=inp } =
  sum . zipWith (*) [1000,100,10,1] $ digits
  where
    known = M.fromList . map swap . toList . _known . decode $ p
    digits = map (known !) inp

-- | uses a line solver @(Puzzle -> Int)@ to solve all the puzzles, summing up each
-- puzzle's solution to give the overall solution
solve :: (Puzzle -> Int) -> Puzzles -> Int
solve fn = sum . map fn

day08a :: Puzzles :~> Int
day08a = MkSol
  { sParse = Just . map parseLine . lines
  , sShow  = show
  , sSolve = Just . solve solveLine1 }

day08b :: Puzzles :~> Int
day08b = MkSol
  { sParse = Just . map parseLine . lines
  , sShow  = show
  , sSolve = Just . solve solveLine2 }