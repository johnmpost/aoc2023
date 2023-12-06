module Day3 where

import Prelude

import Data.Array (any, catMaybes, concat, elem, filter, mapWithIndex, range)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.String (Pattern(..), codePointFromChar, indexOf, length, split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (match)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

type Coordinate = { x :: Int, y :: Int }

type Symbol' = { location :: Coordinate }

type Number' = { value :: Int, leftmostLocation :: Coordinate }

mkSymbol :: Coordinate -> Symbol'
mkSymbol coord = { location: coord }

isSymbol :: Char -> Boolean
isSymbol c = not (c == '.' || (c # codePointFromChar # isDecDigit))

getSymbols :: Array String -> Array Symbol'
getSymbols =
  mapWithIndex
    (\y line -> toCharArray line # mapWithIndex (\x char -> Tuple char { x, y }))
    >>> concat
    >>> filter (fst >>> isSymbol)
    >>> map (snd >>> mkSymbol)

convertMaybeNEA :: forall a. Maybe (NonEmptyArray a) -> Array a
convertMaybeNEA maybeNEA = fromMaybe [] (toArray <$> maybeNEA)

getNumbersInLine :: String -> Array (Tuple Int Int)
getNumbersInLine line =
  line
    # match (unsafeRegex "[0-9]+" global)
    # convertMaybeNEA
    # catMaybes
    # map
        ( \numStr -> Tuple
            (fromString numStr # unsafePartial fromJust)
            (indexOf (Pattern numStr) line # unsafePartial fromJust)
        -- welp, this indexOf will of course find the *first*
        -- instance in the line, not always the correct one
        -- going to have to make an FFI for js's matchall
        )

getNumbers :: Array String -> Array Number'
getNumbers =
  mapWithIndex
    ( \y line ->
        line
          # getNumbersInLine
          # map (\(Tuple value x) -> { value, leftmostLocation: { x, y } })
    )
    >>> concat

getAdjacentCoordinates :: Number' -> Array Coordinate
getAdjacentCoordinates number = do
  let numDigits = number.value # show # length
  let boundingLeft = number.leftmostLocation.x - 1
  let boundingRight = number.leftmostLocation.x + numDigits
  let boundingXs = range boundingLeft (number.leftmostLocation.x + numDigits)
  let
    beforeAfter =
      [ { x: boundingLeft, y: number.leftmostLocation.y }
      , { x: boundingRight, y: number.leftmostLocation.y }
      ]
  let above = boundingXs # map ({ x: _, y: number.leftmostLocation.y - 1 })
  let below = boundingXs # map ({ x: _, y: number.leftmostLocation.y + 1 })
  concat [ beforeAfter, above, below ]

isSymbolAdjacent :: Array Symbol' -> Number' -> Boolean
isSymbolAdjacent symbols =
  getAdjacentCoordinates >>> any (flip elem (map (_.location) symbols))

transform :: String -> String
transform =
  do
    lines <- (split (Pattern "\n"))
    let symbols = getSymbols lines
    let numbers = getNumbers lines
    numbers
      # filter (isSymbolAdjacent symbols)
      # map (_.value)
      # sum
      # show
      # pure

