module Day1 where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "./input-day1.txt"

day1 :: Effect Unit
day1 = log "day 1 result"
