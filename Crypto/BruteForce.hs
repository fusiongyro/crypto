module Crypto.BruteForce (bruteForce) where

import Crypto.Support
import Crypto.Shift
--import Crypto.Viginere

import Control.Arrow

bruteForce :: String -> [(Key, String)]
bruteForce encoded = map (id &&& flip decode encoded) crackedKeys
  where
    -- add calls to the other crack overloads here as they're developed
    crackedKeys = [Key (crack encoded :: Shift)] -- ++
--                  [Key (crack encoded :: Viginere)]

