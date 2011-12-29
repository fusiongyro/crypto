{-# LANGUAGE ExistentialQuantification #-}

module Crypto where

import Data.Char
import Data.Maybe

{-

Cryptography module. Based on exercises from the book.

-}

-- | All encryption codecs are defined thus: there is a key of a
-- particular type, which when given to the encoder or the decoder
-- produces a function from string to string which performs the
-- encoding or decoding for that encryption engine.
class Codec key where
  encode :: key -> String -> String
  decode :: key -> String -> String

data Key = forall a. (Codec a, Show a) => Key a

instance Show Key where
  show (Key a) = "Key (" ++ show a ++ ")"

instance Codec Key where
  encode (Key a) = encode a
  decode (Key a) = decode a

-- | Helper function: define the encoder and decoder for a given key.
codec :: (Codec key) => key -> (String -> String, String -> String)
codec key = (encode key, decode key)


-- The Caesar cypher
newtype Caesar = Caesar Int
    deriving (Show, Eq)

-- | Generate a key for the Caesar cypher.
caesar :: Int -> Caesar
caesar = Caesar

-- | "Rotate" the given character by x letters through the alphabet.
rotate :: Int -> Char -> Char
rotate x c = rot 'a' 'z' (rot 'A' 'Z' c)
    where
      rot low high c = if c >= low && c <= high 
                       then chr $ (ord c - ord low + x) `mod` 26 + ord low
                       else c

instance Codec Caesar where
  encode (Caesar x) = map $ rotate x
  decode (Caesar x) = encode $ Caesar (-x)

-- I vastly prefer the SML name for this function
mapPartial = mapMaybe

-- creates a partial function. the idea here is that the first
-- argument encodes detecting if the function is valid for the input
-- and the second function performs the change. if the function
-- applies, it is applied; otherwise, Nothing.
partialFun :: (a -> Bool) -> (a -> b) -> a -> Maybe b
partialFun cond fn a | cond a = Just $ fn a
partialFun _    _  _          = Nothing

-- converts a partial function to a total function. if the output is
-- of the same type as the input, then if the function does not apply
-- the value is passed through unchanged.
toTotal :: (a -> Maybe a) -> a -> a
toTotal fn x = fromMaybe x (fn x)

newtype Viginere = Viginere String
  deriving (Show, Eq)

viginereKeySequence :: String -> [Int]
viginereKeySequence key = map (\c -> ord (toUpper c) - ord 'A') key

viginereShift :: [Int] -> String -> String
viginereShift key = zipWith rotate (cycle key)

instance Codec Viginere where
  encode (Viginere key) = viginereShift $ viginereKeySequence key
  decode (Viginere key) = viginereShift $ map negate (viginereKeySequence key)

test_Viginere = encode (Viginere "lemon") "attackatdawn" == "lxfopvefrnhr"

testKeys :: [Key]
testKeys = [Key (Viginere "lemon"), Key (Caesar 3)]

-- breaking cryptography
