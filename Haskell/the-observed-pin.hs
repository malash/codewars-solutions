module PIN where

adjust :: Char -> [Char]
adjust '1' = "124"
adjust '2' = "2135"
adjust '3' = "326"
adjust '4' = "4157"
adjust '5' = "52468"
adjust '6' = "6359"
adjust '7' = "748"
adjust '8' = "85790"
adjust '9' = "968"
adjust '0' = "08"

getPINs :: String -> [String]
getPINs xs = mapM adjust xs
