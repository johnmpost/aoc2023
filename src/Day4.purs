module Day4 where

import Prelude

import Data.Foldable (length, sum)
import Data.Int (pow)
import Data.List (List(..), fromFoldable, intersect, reverse, take, (:))
import Data.List.NonEmpty (toList)
import Data.String (Pattern(..), split)
import Parsing (Parser)
import Parsing.Combinators (many1Till, sepBy1)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, whiteSpace)
import Utils (unsafeRunParser)

type Scratchcard = { cardNo :: Int, winningNums :: List Int, haveNums :: List Int }

scratchcard :: Parser String Scratchcard
scratchcard = do
  _ <- string "Card" *> whiteSpace
  cardNo <- intDecimal
  _ <- string ":" *> whiteSpace
  winningNums' <- (intDecimal <* whiteSpace) `many1Till` (string "|") <* whiteSpace
  haveNums' <- intDecimal `sepBy1` whiteSpace
  let winningNums = winningNums' # toList
  let haveNums = haveNums' # toList
  pure { cardNo, winningNums, haveNums }

matchesToPoints :: Int -> Int
matchesToPoints 0 = 0
matchesToPoints n = 2 `pow` (n - 1)

toNumMatches :: Scratchcard -> Int
toNumMatches { winningNums, haveNums } = intersect winningNums haveNums # length

scratchcardToPoints :: Scratchcard -> Int
scratchcardToPoints { winningNums, haveNums } =
  intersect winningNums haveNums
    # length
    # matchesToPoints

-- transform :: String -> String
-- transform =
--   (split (Pattern "\n"))
--     >>> map (unsafeRunParser scratchcard)
--     >>> map scratchcardToPoints
--     >>> sum
--     >>> show

getTotalCards :: List Int -> List Int
getTotalCards = go (Nil)
  where
  go acc (Nil) = reverse acc
  go acc (x : xs) = go (newVal : acc) xs
    where
    newVal = 1 + sum (take x acc)

transform :: String -> String
transform =
  (split (Pattern "\n"))
    >>> map (unsafeRunParser scratchcard)
    >>> map toNumMatches
    >>> fromFoldable
    >>> reverse
    >>> getTotalCards
    >>> sum
    >>> show
