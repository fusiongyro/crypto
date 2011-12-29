module Crypto.Shift ( Shift
                    , shift
                    , caesar
                    ) where

import Crypto.Support
import Crypto.Statistics

import Data.Char
import Data.Function
import Data.List

-- The Shift cypher
newtype Shift = Shift Int
    deriving (Show, Eq)

-- | Generate a key for the Shift cypher.
shift :: Int -> Shift
shift = Shift

-- | The famous "Caesar" cypher
caesar :: Shift
caesar = shift 3

instance Codec Shift where
  decode (Shift x) = encode $ Shift (-x)
  encode (Shift x) = ignoreNonAlphas (rotateChar x)

instance Crackable Shift where
  -- take the best quality key, where the quality is the distance-from-English
  -- of the letter frequency distribution
  crack s = head $ sortBy quality allShiftKeys
    where
      quality      = compare `on` distanceFromEnglish `on` (flip decode s)
      allShiftKeys = map shift [0..25]

tristessesDeLaLune = 
   "   the moon tonight, more indolently dreaming,\n\
\   as on a pillowed bed, a woman seems,\n\
\   caressing with a hand distraught and gleaming,\n\
\   her soft curved bosom, ere she sinks in dreams.\n\
\ \n\    
\   against a snowy satin avalanche\n\
\   she lies entranced and drowned in swooning hours,\n\
\   her gaze upon the visions born to blanch\n\
\   those far blue depths with ever-blossoming flowers.\n\
\ \n\
\   and when in some soft languorous interval,\n\
\   earthward, she lets a stealthy tear-drop fall,\n\
\   a poet, foe to slumber, toiling on,\n\
\ \n\
\   with reverent hollow hand receives the pearl,\n\
\   where shimmering opalescences unfurl,\n\
\   and shields it in his heart, far from the sun."

tristessesEncoded = encode caesar tristessesDeLaLune

test_crack = crack tristessesEncoded == caesar
