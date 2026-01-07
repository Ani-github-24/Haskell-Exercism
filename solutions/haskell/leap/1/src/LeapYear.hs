module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear num = (num `mod` 4 ==0 && num `mod` 100 /= 0) || (num `mod` 400== 0)