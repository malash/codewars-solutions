module Folded.MyLists where

foldListOnce :: [Int] -> [Int]
foldListOnce [] = []
foldListOnce [x] = [x]
foldListOnce xs =
    let mid      = flip div 2 . length $ xs
        (as, bs) = splitAt mid xs
    in  if odd . length $ xs
        then zipWith (+) as (reverse . tail $ bs) ++ [head bs]
        else zipWith (+) as (reverse bs)

foldList :: [Int] -> Int -> [Int]
foldList xs 0 = xs
foldList xs n = foldList (foldListOnce xs) (n - 1)
