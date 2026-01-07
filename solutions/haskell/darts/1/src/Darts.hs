module Darts (score) where

score :: Float -> Float -> Int
score x y 
  | b<= 1 =10
  | b <=5 =5
  | b <=10 =1
  |otherwise =0
  where b = sqrt (x*x + y*y)