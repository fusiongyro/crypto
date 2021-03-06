{-# LANGUAGE ParallelListComp #-}

module Crypto.Statistics ( Frequency
                         , Distribution
                         , distanceFromEnglish
                         , englishFrequencies
                         , frequencies
                         ) where

import Crypto.Support

import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map

type Frequency = (Char, Float)
type Distribution = Map.Map Char Float

-- Frequencies for English letters, taken from 
-- http://en.wikipedia.org/wiki/Letter_frequency
englishFrequencies :: Distribution
englishFrequencies = Map.fromList [('A', 0.11602), 
                      ('B', 0.04702),
                      ('C', 0.03511),	
                      ('D', 0.02670),	
                      ('E', 0.02000),	
                      ('F', 0.03779),	
                      ('G', 0.01950),	
                      ('H', 0.07232),	
                      ('I', 0.06286),	
                      ('J', 0.00631),	
                      ('K', 0.00690),	
                      ('L', 0.02705),	
                      ('M', 0.04374),	
                      ('N', 0.02365),	
                      ('O', 0.06264),	
                      ('P', 0.02545),	
                      ('Q', 0.00173),	
                      ('R', 0.01653),	
                      ('S', 0.07755),	
                      ('T', 0.16671),	
                      ('U', 0.01487),	
                      ('V', 0.00619),	
                      ('W', 0.06661),	
                      ('X', 0.00005),	
                      ('Y', 0.01620),	
                      ('Z', 0.00050)]

distanceFromEnglish :: String -> Float
distanceFromEnglish s = sum distances
  where
    theseFreqs = sortBy (compare `on` fst) basicFreqs
    basicFreqs = Map.toList $ frequencies s `Map.union` initialMap
    initialMap = Map.fromList [ (c, 0) | c <- ['A'..'Z'] ]
    distances = [ abs (thisFreq - engFreq)
                | (_, thisFreq) <- theseFreqs 
                | (_, engFreq) <- Map.toList englishFrequencies ]

-- | Calculate the letter frequency distribution for a given sample of text
frequencies :: String -> Distribution
frequencies sample = Map.fromList map'
    where
      map'    = [ (head l, glength l / len) | l <- upper, isAlpha (head l) ]
      glength = genericLength
      len     = glength sample
      upper   = group $ sort $ map toUpper sample
