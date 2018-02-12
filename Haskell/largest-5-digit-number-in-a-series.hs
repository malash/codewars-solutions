module LargestDigits where

import Data.Ord

maxInt :: String -> String -> String
maxInt x y = case comparing (read :: String -> Int) x y of
    GT -> x
    LT -> y
    EQ -> x

findDigit :: String -> String
findDigit [] = "0"
findDigit xs = take 5 xs `maxInt` (findDigit . tail $ xs)

digit5 :: String -> Int
digit5 xs
    | length xs <= 5 = read xs
    | otherwise      =  read . findDigit $ xs
