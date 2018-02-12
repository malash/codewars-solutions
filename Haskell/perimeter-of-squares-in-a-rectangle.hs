module Codewars.Kata.Perimeter where

perimeter :: Integer -> Integer
perimeter n = fromIntegral $ 4 * (sum . take n' . tail $ fibs) where
  n' = fromInteger(n) + 1
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
