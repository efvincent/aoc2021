module AOC.Challenge.Day07 (day07a, day07b) where

import AOC.Solver ((:~>) (..))
import Data.List (sort)
import Data.List.Split (splitOn)

type CostFn = Int -> [Int] -> Int

-- | cost is simple difference in position and candidate position
cost1 :: CostFn
cost1 candidate =
  sum . map (\pos -> abs (pos - candidate))

-- | cost is simple cost, plus the value described by the @\c1@ function
-- which accounts for each step in the cost increasing in value by one
cost2 :: CostFn
cost2 candidate =
  sum . map ((\c1 -> c1 + ((c1 * c1) - c1) `div` 2) . (\pos -> abs (pos - candidate)))

-- | brute force solution for both parts A and B, where we just check each possible
-- candidate and choose the one with the lowest total cost. I'm sure there's a less
-- heavy handed approach, but this got the job done with less brain math hurt.
-- Note the use of the section technique in @map (`costFn` xs)@, which is the equiv
-- of @map (flip costFn xs)@, but I find more readable
solve :: CostFn -> [Int] -> Maybe Int
solve costFn xs = Just . minimum . map (`costFn` xs) $ [(head xs) .. (last xs)]

day07a :: [Int] :~> Int
day07a = MkSol
  { sParse = Just . sort . map read . splitOn ",",
    sShow = show,
    sSolve = solve cost1 }

day07b :: [Int] :~> Int
day07b = MkSol
  { sParse = Just . sort . map read . splitOn ",",
    sShow = show,
    sSolve = solve cost2 }
