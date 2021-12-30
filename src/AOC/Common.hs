-- |
-- Module      : AOC.Challenge
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Meant to be a place to include common functionality used across
-- different parts in the challenge.
--


module AOC.Common (
  bToi
  ) where

-- | converts a list of bools interpreted as a binary number (ordered from least to 
-- most significant bit) into an Int. Reverse the list first if ordered from most
-- significant to least significant bit
bToi :: [Bool] -> Int
bToi = foldr (\bit acc -> fromEnum bit + 2 * acc) 0