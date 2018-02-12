module Codewars.Kata.NthSeries where

import Text.Printf

seriesSum :: Integer -> String
seriesSum 0 = "0.00"
seriesSum n = printf "%.2f" (result :: Float) where
  result = sum . take (fromIntegral n) $ [1/x | x <- [1,4..]]
