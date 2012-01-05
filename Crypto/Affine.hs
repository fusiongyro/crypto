{-# LANGUAGE TransformListComp #-}

module Crypto.Affine (Affine, affine) where

import Crypto.Partial
import Crypto.Support
import Crypto.Statistics

import Data.Char
import Data.Function
import Data.List

import GHC.Exts

data Affine = Affine Int Int
              deriving (Show, Eq)

affine :: Int -> Int -> Maybe Affine
affine a b = 
  if (a `mod` 26) `gcd` 26 == 1 
  then Just $ Affine (a `mod` 26) (b `mod` 26)
  else Nothing

instance Codec Affine where
  encode aff = ignoreNonAlphas (affineChar aff)
  decode aff = ignoreNonAlphas (deAffineChar (invert aff))

affineChar, deAffineChar :: Affine -> Char -> Char
affineChar (Affine a b)   = intToChar (\c -> c * a + b)
deAffineChar (Affine a b) = intToChar (\c -> (c + b) * a)

invert :: Affine -> Affine
invert (Affine a b) = Affine (modularMultiplicativeInverse a 26) (-b)

test_affine = encode (Affine 5 8) "Affine Cipher" == "Ihhwvc Swfrcp"
test_affine2 = decode (Affine 5 8) (encode (Affine 5 8) "Affine Cipher") == "Affine Cipher"

instance Crackable Affine where
  crack t = [ key
            | key <- allKeys
            , then sortWith  by overallDistance key
            , then takeWhile by overallDistance key <= 0.7
            ]
    where
      allKeys = [ Affine x y | x <- [2..26], y <- [1..26], x `gcd` 26 == 1 ]
      overallDistance key = distanceFromEnglish (decode key t)