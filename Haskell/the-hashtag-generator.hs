module Codewars.Kata.Hashtag where

import Data.Char

generateHashtag :: String -> Maybe String
generateHashtag cs =
    let result =
          concat
          . map (\(x:xs) -> toUpper x : xs)
          . words
          $ cs
        len = length result
    in if len > 0 && len <= 140
        then Just ('#':result)
        else Nothing
