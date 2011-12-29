module Crypto.Affine where

import Crypto.Partial
import Crypto.Support

import Data.Char

data Affine = Affine Int Int
              deriving (Show)

instance Codec Affine where
  encode aff = ignoreNonAlphas (affineChar aff)
  decode aff = ignoreNonAlphas (deAffineChar (invert aff))

affineChar, deAffineChar :: Affine -> Char -> Char
affineChar (Affine a b)   = intToChar (\c -> c * a + b)
deAffineChar (Affine a b) = intToChar (\c -> (c + b) * a)

invert :: Affine -> Affine
invert (Affine a b) = Affine (modularMultiplicativeInverse a 26) (-b)

test_affine = encode (Affine 5 8) "Affine Cipher" == "Ihhwvc Swfrcp"
test_affine2 = (decode (Affine 5 8) $ encode (Affine 5 8) "Affine Cipher") == "Affine Cipher"
