module Codewars.G964.Scramblies where

import Data.List

check :: [Char] -> [Char] -> Bool
check [] _ = True
check _ [] = False
check (x:xs) (y:ys)
    | x == y = check xs ys
    | x < y = False
    | x > y = check (x:xs) ys

scramble :: [Char] -> [Char] -> Bool
scramble s1 s2 = sort s2 `check` sort s1
