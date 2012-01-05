module Tests where

import Control.Applicative

import Test.QuickCheck

import Crypto

instance Arbitrary Shift where
  arbitrary = shift <$> choose (1,25)

prop_shiftEncodes :: Shift -> String -> Bool
prop_shiftEncodes key s = s == "" || encode key s /= s

prop_shiftDecodes :: Shift -> String -> Bool
prop_shiftDecodes key s = decode key (encode key s) == s
