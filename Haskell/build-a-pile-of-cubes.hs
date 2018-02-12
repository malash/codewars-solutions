module Codewars.Kata.PileOfCubes where

findNb :: Integer -> Integer

findNb m
    | div (root * (root + 1)) 2 ^ 2 == m = root
    | otherwise = -1
    where
        intSqrt = floor . sqrt . fromIntegral
        root = intSqrt (intSqrt m * 2)
