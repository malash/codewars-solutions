module CountOnes where

import Debug.Trace

log2 :: Integer -> Integer
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

power2 :: Integer -> Integer
power2 = (2^)

maxAll1 :: Integer -> Integer
maxAll1 n = (power2 . log2 $ n) - 1

isAll1 :: Integer -> Bool
isAll1 n = (maxAll1 $ n + 1) == n

a 0 = 0
a 1 = 1
a n =
    let n' = maxAll1 n
    in if isAll1 n
    then (a n') * 2 + n' + 1
    else n - n' + (a n') + (a $ n - n' - 1)

countOnes :: Integer -> Integer -> Integer
countOnes left right = (a right) - (a $ left - 1)
