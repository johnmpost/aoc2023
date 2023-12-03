module Utils where

import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust maybeVal = case maybeVal of
  Just x -> x
  Nothing -> unsafeCrashWith "Tried to read a Just from a Nothing"
