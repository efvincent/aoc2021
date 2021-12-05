module AOC.Challenge.Day01 (day01a, day01b) where

import AOC.Solver ( (:~>)(..) )

-- | Look at a rolling slice of 2 elements in the array, comparing first
-- and second, acc a counter whenever second is larger than first 
solve1 :: [Int] -> Int
solve1 = go 0
  where
    go n (a:b:rest) = if b > a then go (n+1) (b:rest) else go n (b:rest)
    go n _ = n 

-- | Look at a rolling slice of 3 elements in the list, adding them, and
-- comparing that to the previous window's sum, accumulating where current
-- is greater than previous
solve2 :: [Int] -> Int
solve2 = go 0 Nothing
  where
    go n prv (a:b:c:rest) = 
      let cur = a+b+c in
      case prv of
        Nothing -> go n (Just cur) (b:c:rest)
        Just p -> if cur > p then go (n+1) (Just cur) (b:c:rest) else go n (Just cur) (b:c:rest)
    go n _ _ = n

-- Solution wrappers 

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = Just . map read . lines 
    , sShow  = show
    , sSolve = Just . solve1
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = Just . map read . lines
    , sShow  = show
    , sSolve = Just . solve2
    }
