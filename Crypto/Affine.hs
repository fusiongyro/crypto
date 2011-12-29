module Crypto.Affine where

import Crypto.Partial
import Crypto.Support

import Data.Char

data Affine = Affine Int Int
              deriving (Show)

instance Codec Affine where
  encode aff = map (toTotal (partialFun isAlpha (affineChar aff)))
  decode aff = encode (invert aff)

affineChar :: Affine -> Char -> Char
affineChar = undefined

invert :: Affine -> Affine
invert (Affine a b) = Affine (modularMultiplicativeInverse a 26) (-b)
