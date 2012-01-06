{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}
module Tests where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Text.Printf

import Test.QuickCheck

import Crypto

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests = [ ("encode /= id",          quickCheck prop_encodes)
        , ("decode . encode == id", quickCheck prop_decodes) ]

prop_encodes :: Key -> String -> Bool
prop_encodes key s = encode key s `encodes` s

prop_decodes :: Key -> String -> Bool
prop_decodes key s = decode key (encode key s) == s

encodes :: String -> String -> Bool
encodes x y = x /= y ||                -- they're not the same
  all (not . isAsciiLetter) x ||       -- or, there's no ascii letters here
  length (filter isAsciiLetter x) == 1 -- or, there's only one ascii letter here (key may be 1 in some cases or something)

instance Arbitrary Shift where
  arbitrary = shift <$> choose (1,25)

instance Arbitrary Affine where
  arbitrary = forceAffine <$> elements a <*> elements b
    where
      a = [ x | x <- [1..26], (x `mod` 26) `gcd` 26 == 1 ]
      b = [1..25]

instance Arbitrary Viginere where
  arbitrary = viginere <$> listOf1 (choose ('a', 'z'))

instance Arbitrary Key where
  arbitrary = oneof [arbShift, arbAff, arbVig]
    where
      arbShift = Key <$> (arbitrary :: Gen Shift)
      arbAff   = Key <$> (arbitrary :: Gen Affine)
      arbVig   = Key <$> (arbitrary :: Gen Viginere)
