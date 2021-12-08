module AOC.Challenge.Day06 (day06a, day06b) where

import AOC.Solver ( (:~>)(..) )
import Data.IntMap as IM (insert, empty, (!), elems, delete, mapKeys, updateWithKey)
import Data.List.Split (splitOn)

-- | Builds an @IntMap Int@ with indexes @0..8@, where the index is the number 
-- of iterations until the count down hits zero and the fish "times out", and the 
-- value is the number of fish at that count down. Then at each step, value at 
-- key @0@ is held as the number of fish that have @timedOut@, that pair is
-- removed from the map, and all the indexes are decremented. The @timedOut@ value is
-- added to index @6@, and also becomes the new value at index @8@. Continue this
-- for the number of days requested, and at the end, sum the total of all the counts
solve :: Int -> [Int] -> Int
solve days nums =
  go 0 countDowns
  where
    countDowns = 
      let cd = foldl (\acc d -> IM.insert d 0 acc) IM.empty [0..8] in        
      foldl (\acc d -> IM.insert d ((acc IM.! d)+1) acc) cd nums
    go day cd 
      | day < days =
        let timedOut = cd IM.! 0 in
        let cd' = IM.insert 8 timedOut 
                . IM.updateWithKey (\_ v -> Just $ v + timedOut) 6 
                . IM.mapKeys (subtract 1) 
                . IM.delete 0 $ cd in
        go (day+1) cd'
      | otherwise =
        sum $ IM.elems cd 

day06a :: [Int] :~> Int
day06a = MkSol
    { sParse = Just . map read . splitOn ","
    , sShow  = show
    , sSolve = Just . solve 80
    }

day06b :: _ :~> _
day06b = MkSol
    { sParse = Just . map read . splitOn ","
    , sShow  = show
    , sSolve = Just . solve 256
    }
