module Day1 where

import Prelude

import Data.Array (catMaybes)
import Data.Array.NonEmpty (fromArray, head, last)
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Foldable (sum)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), split, toCodePointArray)
import Partial.Unsafe (unsafePartial)

calibrationValueFromString :: String -> Int
calibrationValueFromString =
  toCodePointArray
    >>> (map decDigitToInt)
    >>> catMaybes
    >>> fromArray
    >>> unsafePartial fromJust
    >>> (\n -> head n * 10 + last n)

transform :: String -> String
transform =
  (split (Pattern "\n"))
    >>> (map calibrationValueFromString)
    >>> sum
    >>> show
