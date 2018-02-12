module Awesome.Numbers where

import Data.List
import Data.Char

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

splitInt :: Integer -> String
splitInt x = show $ x

next :: Char -> Char
next '9' = '0'
next '0' = '!'
next c = succ c

prev :: Char -> Char
prev '0' = '!'
prev c = pred c

all0 :: Char -> Char
all0 = const '0'

isSequential :: (Char -> Char) -> [Char] -> Bool
isSequential f [] = True
isSequential f [x] = True
isSequential f (x:y:xs)
    | (f x) == y = isSequential f (y:xs)
    | otherwise = False

check :: [Integer] -> Integer -> Answer
check awesomePhrases x
    | x < 100 = No
    | x `elem` awesomePhrases = Yes
    | isSequential id . splitInt $ x = Yes
    | isSequential next . splitInt $ x = Yes
    | isSequential prev . splitInt $ x = Yes
    | isSequential all0 . splitInt $ x = Yes
    | (splitInt x) == (reverse . splitInt $ x) = Yes
    | otherwise = No

result :: [Answer] -> Answer
result [Yes, _, _] = Yes
result [_, Yes, _] = Almost
result [_, _, Yes] = Almost
result _ = No

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs =
    result . map (check xs) $ [x .. x + 2]
