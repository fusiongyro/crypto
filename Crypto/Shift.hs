{-# LANGUAGE TransformListComp #-}
module Crypto.Shift ( Shift
                    , shift
                    , caesar
                    ) where

import Crypto.Support
import Crypto.Statistics

import Data.Char
import Data.Function
import Data.List

import GHC.Exts

-- The Shift cypher
data Shift = Shift Int
    deriving (Show, Eq)

-- | Generate a key for the Shift cypher.
shift :: Int -> Shift
shift a = Shift (a `mod` 26)

-- | The famous "Caesar" cypher
caesar :: Shift
caesar = shift 3

instance Codec Shift where
  decode (Shift x) = encode $ Shift (-x)
  encode (Shift x) = ignoreNonAlphas (rotateChar x)

instance Crackable Shift where
  -- take the best quality key, where the quality is the distance-from-English
  -- of the letter frequency distribution
  crack t = [ key 
            | key <- map shift [1..25]
            , then sortWith by distanceFromEnglish $ decode key t
            , then takeWhile by distanceFromEnglish (decode key t) <= 0.7 ]
