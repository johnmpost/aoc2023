module Day1 where

import Prelude

import Control.Alternative ((<|>))
import Data.Array (catMaybes)
import Data.Array.NonEmpty (fromArray, head, last, toArray)
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
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

digitStrToInt :: String -> Maybe Int
digitStrToInt = fromString

wordToInt :: String -> Maybe Int
wordToInt "one" = Just 1
wordToInt "two" = Just 2
wordToInt "three" = Just 3
wordToInt "four" = Just 4
wordToInt "five" = Just 5
wordToInt "six" = Just 6
wordToInt "seven" = Just 7
wordToInt "eight" = Just 8
wordToInt "nine" = Just 9
wordToInt _ = Nothing

parseNumber :: String -> Int
parseNumber str =
  digitStrToInt str <|> wordToInt str
    # unsafePartial fromJust

calibration2 :: String -> Int
calibration2 =
  match (unsafeRegex "(one|two|three|four|five|six|seven|eight|nine|\\d)" global)
    >>> unsafePartial fromJust
    >>> toArray
    >>> catMaybes
    >>> map parseNumber
    >>> fromArray
    >>> unsafePartial fromJust
    >>> (\n -> head n * 10 + last n)

transform :: String -> String
transform =
  (split (Pattern "\n"))
    >>> (map calibration2)
    >>> sum
    >>> show
