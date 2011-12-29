{-# LANGUAGE ExistentialQuantification #-}

module Crypto.Support ( Codec
                      , encode
                      , decode
                      , rotate
                      , Key(..)
                      , Crackable
                      , crack
                      ) where

import Data.Char

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

-- | Defines the set of crackable cryptography.
class Crackable key where
  crack :: String -> key


-- Various utility functions below

-- | "Rotate" the given character by x letters through the alphabet.
rotate :: Int -> Char -> Char
rotate x c = rot 'a' 'z' (rot 'A' 'Z' c)
    where
      rot low high c = if c >= low && c <= high 
                       then chr $ (ord c - ord low + x) `mod` 26 + ord low
                       else c
