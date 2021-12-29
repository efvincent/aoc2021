{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AOC.Challenge.Day16 where -- (day16a, day16b) where

import AOC.Solver ( (:~>)(..) )
import Data.Bifunctor ( Bifunctor(second) )
import qualified Data.Map as M (fromList, keys, (!))

data Packet = Packet 
  { _ver :: Int
  , _typeId :: Int }

data Kind = Literal Int
          | Operator Op [Packet]

data Op = Unnknown 

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
