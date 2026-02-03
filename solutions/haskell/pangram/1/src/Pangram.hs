module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram sentence =
    all (`elem` letters) ['a'..'z']
  where
    letters = map toLower sentence
