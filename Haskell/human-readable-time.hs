module HumanTime where

toFixed2 :: String -> String
toFixed2 [] = "00"
toFixed2 [x] = ['0', x]
toFixed2 xs = xs

humanReadable :: Int -> String
humanReadable x =
    (toFixed2 . show . (flip div 3600) $ x) ++
    ":" ++
    (toFixed2 . show . (flip mod 60) . (flip div 60) $ x) ++
    ":" ++
    (toFixed2 . show . (flip mod 60) $ x)
