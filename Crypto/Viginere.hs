module Crypto.Viginere ( Viginere
                       , viginere
                       ) where

import Crypto.Support

import Data.Char

newtype Viginere = Viginere String
  deriving (Show, Eq)

viginereKeySequence :: String -> [Int]
viginereKeySequence = map $ \c -> ord (toUpper c) - ord 'A'

viginereShift :: [Int] -> String -> String
viginereShift _ [] = []
viginereShift key@(k:ks) (c:cs) = 
    if isAlpha c 
    then alsoUpperCase (rotateChar k) c : viginereShift (ks ++ [k]) cs
    else c : viginereShift key cs

viginere :: String -> Viginere
viginere = Viginere

instance Codec Viginere where
  encode (Viginere key) = viginereShift $ viginereKeySequence key
  decode (Viginere key) = viginereShift $ map negate (viginereKeySequence key)

test_Viginere = encode (Viginere "lemon") "attackatdawn" == "lxfopvefrnhr"
