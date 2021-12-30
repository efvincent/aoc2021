{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AOC.Challenge.Day16 where -- (day16a, day16b) where

import AOC.Solver ( (:~>)(..) )
import AOC.Common (bToi)
import Data.Bifunctor ( Bifunctor(second) )
import qualified Data.Map as M (fromList, keys, (!))
import Numeric (readHex)
import Text.Printf (printf)


data Packet = Packet 
  { _ver :: Int
  , _typeId :: Int }

data Kind = Literal Int
          | Operator Op [Packet]

data Op = Unknown 
        | Add

hexToBin :: Char -> [Bool]
hexToBin c =
  case readHex [c] of
    (x,_):_ -> map (== '1') . printf "%04b" $ (x::Int)
    _       -> error "Invalid input"

parse :: String -> Packet
parse s =
  Packet 0 0
  where
    ver = bToi . concatMap hexToBin . take 3 $ s


day16a :: _ :~> _
day16a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
