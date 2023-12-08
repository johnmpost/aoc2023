module Day4 where

import Prelude

import Data.Array (fromFoldable, (!!))
import Data.Array as A
import Data.Foldable (length)
import Data.Int (pow)
import Data.List (List(..), intersect, range, (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), split)
import Parsing (Parser)
import Parsing.Combinators (many1Till, sepBy1)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, whiteSpace)
import Partial.Unsafe (unsafePartial)
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

processPile :: Array Int -> List Int -> Int -> Int
processPile matchesPerCard pile processedCount =
  case pile of
    (Nil) -> processedCount
    (cardNo : remCards) ->
      let
        numMatches = matchesPerCard !! cardNo # unsafePartial fromJust
        newInts = if numMatches == 0 then (Nil) else range (cardNo + 1) (cardNo + numMatches)
        newPile = remCards <> newInts
      in
        processPile matchesPerCard newPile (processedCount + 1)

transform :: String -> String
transform = do
  cardsByIndex <-
    (split (Pattern "\n"))
      >>> map (unsafeRunParser scratchcard)
      >>> map toNumMatches
      >>> fromFoldable
  let initialPile = range 0 (A.length cardsByIndex - 1)
  let totalNumCards = processPile cardsByIndex initialPile 0
  pure $ show totalNumCards
