module Utils where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity)
import Parsing (ParserT, runParser)
import Partial.Unsafe (unsafeCrashWith)

type Regex = String

foreign import matchAll
  :: Regex
  -> String
  -> Array { match :: String, index :: Int }

unsafeFromRight :: forall a b. Either a b -> b
unsafeFromRight (Right value) = value
unsafeFromRight (Left _) = unsafeCrashWith "used unsafeFromRight on a Left"

unsafeRunParser :: forall a b. ParserT a Identity b -> a -> b
unsafeRunParser parser = flip runParser parser >>> unsafeFromRight
