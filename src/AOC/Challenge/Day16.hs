{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Challenge.Day16 (day16a, day16b) where

import AOC.Solver ( (:~>)(..) )
import AOC.Common (bToi)
import Numeric (readHex)
import Text.Printf (printf)
import Data.Bifunctor (first)

type Bits = [Bool]

data Packet = Packet
  { _ver :: Int
  , _typeId :: Int
  , _kind :: Kind }
  deriving stock (Show)

data Kind = Literal Int
          | Operator Op [Packet]
          deriving stock (Show)

data Op = Sum
        | Product
        | Max
        | Min
        | Greater
        | Less
        | Equal
        deriving stock (Show)

{-
Solutions
-}

-- | Sum the version numbers in packets and their sub packets
sumVersions :: Int -> [Packet] -> Int
sumVersions acc [] = acc
sumVersions acc (p:pkts) = 
  let acc' = acc + _ver p in
  let curSum =
        case _kind p of
        Literal _ -> acc'
        Operator _ subPackets -> sumVersions acc' subPackets in
  sumVersions curSum pkts    

eval :: Packet -> Int 
eval p =
  case _kind p of 
    Literal n -> n
    Operator op pkts -> evalOp (map eval pkts) op
  where
    evalOp vals Sum     = sum vals
    evalOp vals Product = product vals
    evalOp vals Min     = minimum vals
    evalOp vals Max     = maximum vals
    evalOp vals Greater = let [v1,v2] = vals in if v1 >  v2 then 1 else 0
    evalOp vals Less    = let [v1,v2] = vals in if v1 <  v2 then 1 else 0
    evalOp vals Equal   = let [v1,v2] = vals in if v1 == v2 then 1 else 0

day16x :: (Packet -> Int) -> Packet :~> Int
day16x fn = MkSol { sParse = Just . parse, sShow  = show, sSolve = Just . fn }

day16a :: Packet :~> Int
day16a = day16x $ sumVersions 0 . (:[])

day16b :: Packet :~> Int
day16b = day16x eval

{-
Parsing
-}

parse :: String -> Packet
parse s =
  fst $ parsePacket ver typ rest'
  where
    (ver, rest) = first (binToInt . padBin 4) . splitAt 3 . decodeHex $ s
    (typ, rest') = first (binToInt . padBin 4) . splitAt 3 $ rest

parsePacket :: Int -> Int -> Bits -> (Packet, Bits)
parsePacket ver 4 rest =
  let (value, remainder) = getLiteral [] rest in
  (Packet ver 4 (Literal $ binToInt value), remainder)
  where
    getLiteral :: Bits -> Bits -> (Bits, Bits)
    getLiteral acc (cont:b1:b2:b3:b4:bs) = 
      let acc' = (acc ++ [b1, b2, b3, b4]) in
      if cont then getLiteral acc' bs else (acc',bs)
    getLiteral _ _ = error "invalid input"
parsePacket ver typ rest =
  if lt then
    -- next 11 bits contain the number of sub-packets to expect
    let (packetCount, postLenBits) = first binToInt . splitAt 11 $ rest' in 
    let (packets,remaining) = takePackets packetCount postLenBits [] in
    (Packet ver typ (Operator (getOpType typ) packets), remaining)
  else
    -- next 15 bits contain the sub-packet length
    let (subLen, postLenBits) = first binToInt . splitAt 15 $ rest' in
    let (subBits, postSubBits) = splitAt subLen postLenBits in
    let packets = consume subBits [] in
    (Packet ver typ (Operator (getOpType typ) packets), postSubBits)
  where
    (lt:rest') = rest 
    takePackets :: Int -> Bits -> [Packet] -> ([Packet], Bits)
    takePackets pcount bits pkts
      | pcount == 0 = (reverse pkts, bits)
      | otherwise =
        let (p,remaining) = parseBits bits in
        takePackets (pcount - 1) remaining (p:pkts)
    consume :: Bits -> [Packet] -> [Packet]
    consume [] pkts = reverse pkts
    consume bs acc =
      let (pkt, remaining) = parseBits bs in
      consume remaining (pkt:acc)
    parseBits :: Bits -> (Packet,Bits)
    parseBits bits =
      let (v, bits') = first (binToInt . padBin 4) . splitAt 3 $ bits in
      let (t, bits'') = first (binToInt . padBin 4) . splitAt 3 $ bits' in
      parsePacket v t bits''     

{-
Utility functions
-}

getOpType :: Int -> Op
getOpType 0 = Sum
getOpType 1 = Product
getOpType 2 = Min
getOpType 3 = Max
getOpType 5 = Greater
getOpType 6 = Less
getOpType 7 = Equal
getOpType n = error $ "invalid operator type value " ++ show n

binToInt :: Bits -> Int
binToInt = bToi . reverse
    
decodeHex :: String -> Bits
decodeHex = concatMap decode
  where
    decode :: Char -> Bits
    decode c =
      case readHex [c] of
        (x,_):_ -> map (== '1') . printf "%04b" $ (x::Int)
        _       -> error "Invalid input"

padBin :: Int -> Bits -> Bits 
padBin n bin =
  case n - length bin of 
    diff | diff > 0 -> replicate diff False ++ bin
    _ -> bin