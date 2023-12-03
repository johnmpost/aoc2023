module Day1 where

import Prelude

import Data.Array (catMaybes)
import Data.Array.NonEmpty (fromArray, head, last)
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Number.Format (toString)
import Data.String (Pattern(..), split, toCodePointArray)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Utils (readTextFileSafe, unsafeFromJust)

input :: Effect String
input = readTextFileSafe UTF8 "./src/input-day1.txt" # map unsafeFromJust

calibrationValueFromString :: String -> Number
calibrationValueFromString = toCodePointArray
  >>> (map decDigitToInt)
  >>> catMaybes
  >>> fromArray
  >>> unsafeFromJust
  >>> (\n -> head n * 10 + last n)
  >>> toNumber

transform :: String -> String
transform = (split (Pattern "\n"))
  >>> (map calibrationValueFromString)
  >>> sum
  >>> toString

day1 :: Effect Unit
day1 = input
  # map transform
  >>= log
