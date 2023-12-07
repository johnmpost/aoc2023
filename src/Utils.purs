module Utils where

type Regex = String

foreign import matchAll
  :: Regex
  -> String
  -> Array { match :: String, index :: Int }
