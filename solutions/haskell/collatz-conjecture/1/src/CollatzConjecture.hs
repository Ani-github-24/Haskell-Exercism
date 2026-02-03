module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0    = Nothing
    | otherwise = Just (steps n)

steps :: Integer -> Integer
steps 1 = 0
steps n
    | even n    = 1 + steps (n `div` 2)
    | otherwise = 1 + steps (3 * n + 1)
