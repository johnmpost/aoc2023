module Day5 where

import Prelude

import Data.Foldable (find, foldl, minimum)
import Data.List (List)
import Data.Maybe (fromMaybe)
import Parsing (Parser)
import Parsing.Combinators (many, manyTill, sepBy)
import Parsing.String (anyChar, string)
import Parsing.String.Basic (number)
import Partial.Unsafe (unsafeCrashWith)
import Utils (unsafeRunParser)

type Range = { destStart :: Number, sourceStart :: Number, length :: Number }

type Map' = List Range

type Almanac = { seeds :: List Number, maps :: List Map' }

range' :: Parser String Range
range' = do
  destStart <- number <* string " "
  sourceStart <- number <* string " "
  length <- number
  pure { destStart, sourceStart, length }

map' :: Parser String Map'
map' = do
  _ <- manyTill anyChar (string "map:\n")
  ranges <- manyTill (range' <* string "\n") (string "\n")
  pure ranges

almanac :: Parser String Almanac
almanac = do
  seeds <- string "seeds: " *> number `sepBy` (string " ")
  maps <- many map'
  pure { seeds, maps }

isInRange :: Number -> Range -> Boolean
isInRange num { sourceStart, length } = num >= sourceStart && num < sourceStart + length

applyMap :: Number -> Map' -> Number
applyMap num map'' = do
  let range'' = find (isInRange num) map''
  let mapVal = range'' # map (_.destStart - _.sourceStart)
  let mapValReal = fromMaybe 0.0 mapVal
  num + mapValReal

seedToLocation :: List Map' -> Number -> Number
seedToLocation = flip (foldl applyMap)

transform :: String -> String
transform = do
  { seeds, maps } <- unsafeRunParser almanac
  let locations = map (seedToLocation maps) seeds
  let startLocation = minimum locations
  pure $ show startLocation

