module Day1 where

import Prelude

import Control.Alternative ((<|>))
import Data.Array (catMaybes)
import Data.Array.NonEmpty (fromArray, head, last)
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), split, toCodePointArray)
import Data.String.Regex (match)
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)

part1GetCalibration :: String -> Int
part1GetCalibration =
  toCodePointArray
    >>> (map decDigitToInt)
    >>> catMaybes
    >>> fromArray
    >>> unsafePartial fromJust
    >>> (\n -> head n * 10 + last n)

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

strDigitOrWordToInt :: String -> Int
strDigitOrWordToInt str =
  fromString str <|> wordToInt str
    # unsafePartial fromJust

getOneMatch :: String -> String -> String
getOneMatch pattern =
  match (unsafeRegex pattern noFlags)
    >>> unsafePartial fromJust
    >>> head
    >>> unsafePartial fromJust

part2GetCalibration :: String -> Int
part2GetCalibration line = do
  let firstPattern = "([1-9]|one|two|three|four|five|six|seven|eight|nine)"
  let lastPattern = "(?<=([1-9]|one|two|three|four|five|six|seven|eight|nine))$"
  let firstMatch = getOneMatch firstPattern line # strDigitOrWordToInt
  let lastMatch = getOneMatch lastPattern line # strDigitOrWordToInt
  firstMatch * 10 + lastMatch

transform :: String -> String
transform =
  (split (Pattern "\n"))
    >>> (map part2GetCalibration)
    >>> sum
    >>> show
