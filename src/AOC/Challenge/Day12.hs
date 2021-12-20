{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day12 where -- (day12a, day12b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, (!), empty, findWithDefault, fromList, keys, adjust, insert, elems)
import Data.Set as S (Set, toList, empty, insert, member, elems)
import Data.Data (Data(dataCast2))
import Data.List.Split (splitOn)
import Data.Char (isLower)

data CaveType = Start | End | Small | Large deriving stock (Show, Eq, Ord)

data Cave = Cave
  { _lbl :: String
  , _type :: CaveType
  , _nbrs :: Set String }
  deriving stock (Show)

instance Eq Cave where
  (==) c1 c2 = _lbl c1 == _lbl c2

instance Ord Cave where
  compare c1 c2 = compare (_lbl c1) (_lbl c2)

caveType :: String -> CaveType
caveType "start" = Start
caveType "end" = End
caveType s = case s of
  (c:_) | isLower c -> Small
  _ -> Large

parse :: String -> Map String Cave
parse s =
  foldr (\(a,b) m -> 
    let c = findWithDefault (Cave a (caveType a) S.empty) a m in
    let c' = c { _nbrs = S.insert b (_nbrs c) } in
    M.insert a c' m) M.empty cs2
  where
    cs1 = map ((\[a,b] -> (a,b)) . splitOn "-") . lines $ s
    cs2 = cs1 ++ map (\(a,b) -> (b,a)) cs1

day12a :: _ :~> _
day12a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day12b :: _ :~> _
day12b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
