module Crypto.Substitution ( Substitution
                           , substitution
                           ) where

import Crypto.Partial
import Crypto.Support
import Crypto.Statistics

import Data.Char
import Data.Function
import Data.List
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

-- some utilities we need for cracking

-- given two distributions, try to infer a substitution key based on
-- the nearest keys.

-- | pick : choose the character in this distribution with the nearest
-- frequency rate to the supplied character/rate.
type Frequency = (Char, Float)
type Distribution = [Frequency]

pick :: Frequency -> Distribution -> (Frequency, Distribution)
pick (_, freq) l = (winner, delete winner l)
    where
      winner   = head $ sortBy nearness l
      nearness (_, lfreq) (_, rfreq) = abs (lfreq - freq) `compare` (abs (rfreq - freq))

guessDistribution :: Distribution -> [(Char, Char)]
guessDistribution d = go d englishFrequencies
  where
    go (freq@(c,f):freqs) distrib = (c, out) : rest 
        where
          ((out, _), remaining) = pick freq distrib
          rest = go freqs remaining
