{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day12 (day12a, day12b) where

import AOC.Solver ( (:~>)(..) )
import Data.Map as M (Map, (!), empty, findWithDefault, insert)
import Data.Set as S (Set, toList, empty, insert)
import Data.List.Split (splitOn)
import Data.Char (isLower)

type Caves = Map String Cave

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

parse :: String -> Caves
parse s =
  foldr (\(a,b) m ->
    let c = findWithDefault (Cave a (caveType a) S.empty) a m in
    let c' = c { _nbrs = S.insert b (_nbrs c) } in
    M.insert a c' m) M.empty cs2
  where
    cs1 = map ((\[a,b] -> (a,b)) . splitOn "-") . lines $ s
    cs2 = cs1 ++ map (\(a,b) -> (b,a)) cs1

subPathsToEnd :: String -> Bool -> [String] -> Caves -> [[String]]
subPathsToEnd curId noDoubleSmall path caves =
  let cur = caves!curId in
  concatMap (\subId ->
    case caveType subId of
      End -> [subId : path]
      ct | ct == Start -> []
      ct | ct == Small && (subId `elem` path) -> 
        if noDoubleSmall
        then [] 
        else subPathsToEnd subId True (subId:path) caves 
      _ -> subPathsToEnd subId noDoubleSmall (subId : path) caves
  ) (S.toList . _nbrs $ cur)

day12x :: Show a => (Caves -> a) -> Caves :~> a
day12x fn = MkSol { sParse = Just . parse, sShow = show, sSolve = Just . fn }

day12a :: Caves :~> Int
day12a = day12x $ length . subPathsToEnd "start" True ["start"]

day12b :: Caves :~> Int
day12b = day12x $ length . subPathsToEnd "start" False ["start"]
