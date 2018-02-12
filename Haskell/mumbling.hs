module Codewars.G964.Accumule where

import Data.Char

format :: Int -> Char -> [Char]
format n char = (toUpper char) : (take (n-1) . repeat . toLower $ char)

join :: Char -> [String] -> String
join _ [] = []
join _ [x] = x
join sep (x:xs) = x ++ [sep] ++ (join sep xs)

accum :: [Char] -> [Char]
accum s = join '-' . zipWith format [1..] $ s
