{-# LANGUAGE ExistentialQuantification #-}

module Crypto.Support ( Codec
                      , encode
                      , decode
                      , rotateChar
                      , codec
                      , Key(..)
                      , Crackable
                      , crack
                      , modularMultiplicativeInverse
                      , alsoUpperCase
                      , ignoreNonAlphas
                      , intToChar
                      ) where

import Crypto.Partial

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

intToChar :: (Int -> Int) -> Char -> Char
intToChar f c = chr $ (f (ord c - ord 'a')) `mod` 26 + ord 'a'

-- | "Rotate" the given character by x letters through the alphabet.
rotateChar :: Int -> Char -> Char
rotateChar x = intToChar (+x)

-- | alsoUpperCase - if f changes a lowercase character, make it work
-- for upper case characters as well.
alsoUpperCase :: (Char -> Char) -> Char -> Char
alsoUpperCase f c = if isUpper c 
                    then toUpper $ f $ toLower c
                    else f c

-- | ignoreNonAlphas - make f work on both cases and non-alphabetic chars by
-- ignoring them.
ignoreNonAlphas :: (Char -> Char) -> String -> String
ignoreNonAlphas f = map $ toTotal $ partialFun isAlpha (alsoUpperCase f)

-- modular arithmetic helpers
extendedGcd a 0 = (1, 0)
extendedGcd a b = (t, s - q * t)
  where
    (q, r) = a `divMod` b
    (s, t) = extendedGcd b r

modularMultiplicativeInverse a m = t `mod` m
    where
      (t, _) = extendedGcd a m
