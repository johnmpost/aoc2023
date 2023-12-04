module Day3 where

import Prelude

import Data.Array (catMaybes, concat, filter, mapWithIndex)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.String (Pattern(..), codePointFromChar, indexOf, split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (match)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

type Coordinate = { x :: Int, y :: Int }

type Symbol' = { location :: Coordinate }

type Number' = { value :: Int, leftmostLocation :: Coordinate }

mkSymbol :: Coordinate -> Symbol'
mkSymbol coord = { location: coord }

isSymbol :: Char -> Boolean
isSymbol char = char /= '.' && (char # codePointFromChar # isDecDigit # not)

getSymbols :: Array String -> Array Symbol'
getSymbols =
  mapWithIndex
    (\y line -> toCharArray line # mapWithIndex (\x char -> Tuple char { x, y }))
    >>> concat
    >>> filter (\(Tuple char _) -> isSymbol char)
    >>> map (snd >>> mkSymbol)

convertMaybeNEA :: forall a. Maybe (NonEmptyArray a) -> Array a
convertMaybeNEA maybeNEA = fromMaybe [] (toArray <$> maybeNEA)

getNumbersInLine :: String -> Array (Tuple Int Int)
getNumbersInLine line =
  line
    # match (unsafeRegex "intregex" global)
    # convertMaybeNEA
    # catMaybes
    # map
        ( \numStr -> Tuple
            (fromString numStr # unsafePartial fromJust)
            (indexOf (Pattern numStr) line # unsafePartial fromJust)
        )

getNumbers :: Array String -> Array Number'
getNumbers =
  mapWithIndex
    (\y line -> line # getNumbersInLine # map (\(Tuple value x) -> { value, leftmostLocation: { x, y } }))
    >>> concat

isSymbolAdjacent :: Array Symbol' -> Number' -> Boolean
isSymbolAdjacent = unsafeCrashWith ""

transform :: String -> String
transform =
  do
    lines <- (split (Pattern "\n"))
    let symbols = getSymbols lines
    let numbers = getNumbers lines
    numbers
      # filter (isSymbolAdjacent symbols)
      # sum
      # show
      # pure

