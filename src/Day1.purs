module Day1 where

import Prelude

import Data.Array (catMaybes)
import Data.Array.NonEmpty (fromArray, head, last)
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Number.Format (toString)
import Data.String (Pattern(..), split, toCodePointArray)
import Partial.Unsafe (unsafePartial)

calibrationValueFromString :: String -> Number
calibrationValueFromString =
  toCodePointArray
    >>> (map decDigitToInt)
    >>> catMaybes
    >>> fromArray
    >>> unsafePartial fromJust
    >>> (\n -> head n * 10 + last n)
    >>> toNumber

transform :: String -> String
transform =
  (split (Pattern "\n"))
    >>> (map calibrationValueFromString)
    >>> sum
    >>> toString
