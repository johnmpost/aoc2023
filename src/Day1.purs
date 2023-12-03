module Day1 where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafeCrashWith)

readTextFileSafe :: Encoding -> FilePath -> Effect (Maybe String)
readTextFileSafe encoding =
  readTextFile encoding
    >>> try
    >>> map (either (const Nothing) Just)

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust maybeVal = case maybeVal of
  Just x -> x
  Nothing -> unsafeCrashWith ""

input :: Effect String
input = readTextFileSafe UTF8 "./src/input-day1.txt" # map unsafeFromJust

day1 :: Effect Unit
day1 = input >>= log
