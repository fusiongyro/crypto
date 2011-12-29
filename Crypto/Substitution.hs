module Crypto.Substitution ( Substitution
                           , substitution
                           ) where

import Crypto.Partial
import Crypto.Support

import Data.Char
import qualified Data.Map as Map

data Substitution = Substitution String
                    deriving (Show, Eq)

substitution :: String -> Substitution
substitution = Substitution

generateMapping :: String -> Map.Map Char Char
generateMapping str = Map.fromList $ zip ['a'..'z'] str

substituteChars :: Map.Map Char Char -> String -> String
substituteChars key = ignoreNonAlphas enc
    where 
      enc c = Map.findWithDefault c c key

invert :: (Ord a) => Map.Map a a -> Map.Map a a
invert map = Map.fromList [ (val, key) | (key, val) <- Map.toList map ]

instance Codec Substitution where
  encode (Substitution key) = substituteChars (generateMapping key)
  decode (Substitution key) = substituteChars (invert (generateMapping key))
