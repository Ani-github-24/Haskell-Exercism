module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isLetter)

responseFor :: String -> String
responseFor input
    | isSilence trimmed = "Fine. Be that way!"
    | isYelling trimmed && isQuestion trimmed = "Calm down, I know what I'm doing!"
    | isYelling trimmed = "Whoa, chill out!"
    | isQuestion trimmed = "Sure."
    | otherwise = "Whatever."
  where
    trimmed = trim input

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

isSilence :: String -> Bool
isSilence s = null s

isQuestion :: String -> Bool
isQuestion s = not (null s) && last s == '?'

isYelling :: String -> Bool
isYelling s =
    any isLetter s && all (\c -> not (isLetter c) || isUpper c) s
