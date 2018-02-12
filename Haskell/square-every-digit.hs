module SquareDigit where
import Data.Char

square :: String -> String
square [] = []
square (x:xs) = (show $ d * d) ++ (square xs) where
  d = read [x]


squareDigit :: Int -> Int
squareDigit x
  | x < 0 = -(squareDigit (0-x))
  | otherwise = read . square . show $ x
