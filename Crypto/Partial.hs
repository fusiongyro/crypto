module Crypto.Partial where

import Data.Maybe

-- I vastly prefer the SML name for this function
mapPartial = mapMaybe

-- creates a partial function. the idea here is that the first
-- argument encodes detecting if the function is valid for the input
-- and the second function performs the change. if the function
-- applies, it is applied; otherwise, Nothing.
partialFun :: (a -> Bool) -> (a -> b) -> a -> Maybe b
partialFun cond fn a | cond a = Just $ fn a
partialFun _    _  _          = Nothing

-- converts a partial function to a total function. if the output is
-- of the same type as the input, then if the function does not apply
-- the value is passed through unchanged.
toTotal :: (a -> Maybe a) -> a -> a
toTotal fn x = fromMaybe x (fn x)
