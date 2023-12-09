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

unsafeFromRight :: forall t7 a8. Show a8 => Show t7 => Either a8 t7 -> t7
unsafeFromRight (Right value) = value
unsafeFromRight e = unsafeCrashWith $ show e

unsafeRunParser :: forall b17 d19. Show d19 => ParserT b17 Identity d19 -> b17 -> d19
unsafeRunParser parser = flip runParser parser >>> unsafeFromRight
