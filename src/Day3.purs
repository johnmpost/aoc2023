module Day3 where

import Prelude

import Data.Array (any, concat, elem, filter, mapMaybe, mapWithIndex, range)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (Pattern(..), codePointFromChar, length, split)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)
import Utils (matchAll)

type Coordinate = { x :: Int, y :: Int }

type Symbol' = { location :: Coordinate }

type Number' = { value :: Int, leftmostLocation :: Coordinate }

type Gear = { ratio :: Int }

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

getStars :: Array String -> Array Symbol'
getStars =
  mapWithIndex
    (\y line -> toCharArray line # mapWithIndex (\x char -> Tuple char { x, y }))
    >>> concat
    >>> filter (fst >>> ((==) '*'))
    >>> map (snd >>> mkSymbol)

convertMaybeNEA :: forall a. Maybe (NonEmptyArray a) -> Array a
convertMaybeNEA maybeNEA = fromMaybe [] (toArray <$> maybeNEA)

getNumbersInLine :: String -> Array (Tuple Int Int)
getNumbersInLine line =
  line
    # matchAll "[0-9]+"
    # map \x -> Tuple
        (x.match # fromString # unsafePartial fromJust)
        (x.index)

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

arrayToTuple :: forall a. Array a -> Maybe (Tuple a a)
arrayToTuple [ a, b ] = Just $ Tuple a b
arrayToTuple _ = Nothing

parseGear :: Array Number' -> Symbol' -> Maybe Gear
parseGear numbers star =
  numbers
    # map toValueWithBoundingBox
    # filter starIsAdjacent
    # map fst
    # arrayToTuple
    # map mkGear
  where
  mkGear (Tuple a b) = { ratio: a * b }
  starIsAdjacent (Tuple _ bb) = elem star.location bb
  toValueWithBoundingBox n = Tuple n.value (getAdjacentCoordinates n)

-- transform :: String -> String
-- transform =
--   do
--     lines <- (split (Pattern "\n"))
--     let symbols = getSymbols lines
--     let numbers = getNumbers lines
--     numbers
--       # filter (isSymbolAdjacent symbols)
--       # map (_.value)
--       # sum
--       # show
--       # pure

transform :: String -> String
transform =
  do
    lines <- (split (Pattern "\n"))
    let stars = getStars lines
    let numbers = getNumbers lines
    stars
      # mapMaybe (parseGear numbers)
      # sum
      # show
      # pure
