module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import AOC.Solver ( (:~>)(..) )

-- | converts a list of bools interpreted as a binary number into an Int
bToi :: [Bool] -> Int
bToi = foldr (\x y -> fromEnum x + 2*y) 0

-- | returns the number of elements needed to make a majority in a list
majority :: [_] -> Int
majority vs = 
  let l = length vs in 
  (l `div` 2) + (l `mod` 2)

-- | returns true if the majority of elements at position idx in each element of
-- a list of lists pass the predicate
colWise :: (a -> Bool) -> [[a]] -> Int -> Bool
colWise predicate xs idx =
  (>= majority xs) . length . filter predicate . map (!! idx) $ xs

solve1 :: [[Bool]] -> Int
solve1 values =
  bToi g * bToi e
  where
    l = (length . head $ values) - 1
    g = map (colWise id values) [l,(l-1)..0]
    e = map not g

solve2 :: [[Bool]] -> Int
solve2 values =
  let ox = go id  0 values in
  let co = go not 0 values in
  ox * co
  where
    go :: (Bool -> Bool) -> Int -> [[Bool]] -> Int
    go f i xs =
      let predicate v = (== (v!!i)) . f . (>= majority xs) . length . filter (!! i) $ xs in
      case filter predicate xs of
        [bs] -> bToi . reverse $ bs
        bss -> go f (i+1) bss

day03a :: [[Bool]] :~> Int
day03a = MkSol
    { sParse = Just . map (map (== '1')) . lines
    , sShow  = show
    , sSolve = Just . solve1
    }

day03b :: [[Bool]] :~> Int
day03b = MkSol
    { sParse = Just . map (map (== '1')) . lines
    , sShow  = show
    , sSolve = Just . solve2
    }
