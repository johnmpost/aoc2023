module Day1 where

import Prelude

import Data.Array (catMaybes)
import Data.Array.NonEmpty (fromArray, head, last, toArray)
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Foldable (sum)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), split, toCodePointArray)
import Data.String.Regex (match)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)

calibrationValueFromString :: String -> Int
calibrationValueFromString =
  toCodePointArray
    >>> (map decDigitToInt)
    >>> catMaybes
    >>> fromArray
    >>> unsafePartial fromJust
    >>> (\n -> head n * 10 + last n)

calibration2 :: String -> Int
calibration2 =
  match (unsafeRegex "(one|two)" global)
    >>> unsafePartial fromJust
    >>> toArray
    >>> catMaybes
    >>> fromArray
    >>> unsafePartial fromJust
    >>> \s -> 0

transform :: String -> String
transform =
  (split (Pattern "\n"))
    >>> (map calibrationValueFromString)
    >>> sum
    >>> show
