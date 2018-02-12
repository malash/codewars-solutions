module Codewars.Kata.Arithmetic where

import Data.List

findMissing :: Integral a => [a] -> a
findMissing xs' =
    let xs = take 10000 xs'
        h = head xs
        l = last xs
        step = (l - h) `div` (fromIntegral $ length xs)
        arr = [h, h + step .. l]
    in head (arr \\ xs)
