module Main where

import Prelude

import Day3 (transform)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main =
  "./src/input-day3.txt"
    # readTextFile UTF8
    # map transform
    >>= log