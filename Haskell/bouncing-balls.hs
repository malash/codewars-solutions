module Codewars.Kata.BouncingBall where

clca :: Double -> Double -> Double -> Integer
clca h b w =
    let nextH = h * b
    in  if nextH <= w
        then 0
        else 2 + clca nextH b w

bouncingBall :: Double -> Double -> Double -> Integer
bouncingBall h b w
    | h <= 0 = -1
    | b >= 1 = -1
    | b <= 0 = -1
    | w >= h = -1
    | otherwise = 1 + clca h b w
