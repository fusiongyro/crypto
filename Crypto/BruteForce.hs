{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Crypto.BruteForce (bruteForce) where

import Crypto.Support
import Crypto.Shift
import Crypto.Affine

import Control.Arrow

class BruteForceResult a where
  bruteForce :: String -> a

instance BruteForceResult [Key] where
  bruteForce encoded = crackedKeys 
    where
      crackedKeys = shiftKeys ++ affineKeys
      -- add calls to the other crack overloads here as they're developed
      shiftKeys  = map Key (crack encoded :: [Shift])
      affineKeys = map Key (crack encoded :: [Affine])      

instance BruteForceResult (Key, String) where
  bruteForce encoded = head $ bruteForce encoded

instance BruteForceResult [(Key, String)] where
  bruteForce encoded = map (id &&& flip decode encoded) (bruteForce encoded :: [Key])

instance BruteForceResult Key where
  bruteForce encoded = head $ bruteForce encoded

instance BruteForceResult [String] where
  bruteForce encoded = map snd $ (bruteForce encoded :: [(Key, String)])

instance BruteForceResult String where
  bruteForce encoded = head $ bruteForce encoded
