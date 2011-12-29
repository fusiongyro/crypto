module Crypto.Viginere 
  ( Viginere
  , viginere) where

import Crypto.Support

newtype Viginere = Viginere String
  deriving (Show, Eq)

viginereKeySequence :: String -> [Int]
viginereKeySequence key = map (\c -> ord (toUpper c) - ord 'A') key

viginereShift :: [Int] -> String -> String
viginereShift key = zipWith rotate (cycle key)

viginere :: String -> Viginere
viginere s = Viginere s

instance Codec Viginere where
  encode (Viginere key) = viginereShift $ viginereKeySequence key
  decode (Viginere key) = viginereShift $ map negate (viginereKeySequence key)

test_Viginere = encode (Viginere "lemon") "attackatdawn" == "lxfopvefrnhr"
