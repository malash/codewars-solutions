module PalindromeChain where

reverseInt :: Integer -> Integer
reverseInt x = read . reverse . show $ x

isPalindrome :: Integer -> Bool
isPalindrome x = x == reverseInt x

palindromeChainLength :: Integer -> Integer
palindromeChainLength n
    | isPalindrome n = 0
    | otherwise = 1 + palindromeChainLength (n + reverseInt n)
