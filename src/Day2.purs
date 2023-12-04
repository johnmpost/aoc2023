module Day2 where

import Prelude

import Data.Array (filter, fromFoldable)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (fromJust)
import Data.Semigroup.Foldable (maximum)
import Data.String (Pattern(..), split)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy, (<|>))
import Parsing.String (string)
import Parsing.String.Basic (intDecimal)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

type Round = { red :: Int, blue :: Int, green :: Int }

type Game = { id :: Int, rounds :: NonEmptyArray Round }

type GameMax = Round

type GameMaxes = { id :: Int, maxes :: GameMax }

type Bag = Round

color :: Parser String String
color = string "red" <|> string "green" <|> string "blue"

colorCount :: Parser String { color :: String, count :: Int }
colorCount = do
  count <- intDecimal
  _ <- string " "
  thisColor <- color
  pure { color: thisColor, count }

round :: Parser String Round
round = do
  colorCounts <- colorCount `sepBy` string ", "
  let countColor myColor = sum $ map (_.count) $ filter (\x -> x.color == myColor) (fromFoldable colorCounts)
  pure { red: countColor "red", blue: countColor "blue", green: countColor "green" }

game :: Parser String Game
game = do
  _ <- string "Game "
  gameId <- intDecimal
  _ <- string ": "
  roundsList <- round `sepBy` string "; "
  let rounds = roundsList # fromFoldable # fromArray # unsafePartial fromJust
  pure { id: gameId, rounds: rounds }

roundsMaxes :: NonEmptyArray Round -> GameMax
roundsMaxes rounds =
  { red: rounds # map (_.red) # maximum
  , blue: rounds # map (_.blue) # maximum
  , green: rounds # map (_.green) # maximum
  }

isPossible :: Bag -> GameMaxes -> Boolean
isPossible bag maxes =
  bag.red >= maxes.maxes.red
    && bag.blue >= maxes.maxes.blue
    && bag.green >= maxes.maxes.green

unsafeFromRight :: forall a b. Either a b -> b
unsafeFromRight (Right value) = value
unsafeFromRight (Left _) = unsafeCrashWith "was not Right"

parseGame :: String -> Game
parseGame = flip runParser game >>> unsafeFromRight

transform :: String -> String
transform =
  (split (Pattern "\n"))
    >>> (map $ parseGame)
    >>> (map \g -> { id: g.id, maxes: (roundsMaxes g.rounds) })
    >>> filter (isPossible { red: 12, green: 13, blue: 14 })
    >>> (map _.id)
    >>> sum
    >>> show
