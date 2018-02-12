module FormatDuration where

import Data.Char

instance Show Unit where
    show Second = "second"
    show Minute = "minute"
    show Hour   = "hour"
    show Day    = "day"
    show Year   = "year"

data Unit = Second | Minute | Hour | Day | Year deriving (Eq, Ord, Enum)
type TimeUnit = (Int, Unit)
type Time = [TimeUnit]

getDivMod :: Int -> Unit -> (Int, Int)
getDivMod n Second = n `divMod` 60
getDivMod n Minute = n `divMod` 60
getDivMod n Hour   = n `divMod` 24
getDivMod n Day    = n `divMod` 365
getDivMod n Year   = error "bad"

getDate :: Int -> Unit -> Time
getDate 0 unit = []
getDate x Year = [(x, Year)]
getDate n unit =
    let (q, r) = getDivMod n unit
        next = getDate q (succ unit)
    in if r == 0 then next else (r, unit):next

formatUnit :: TimeUnit -> String
formatUnit (1, unit) = (show 1) ++ " " ++ (show unit)
formatUnit (x, unit) = (show x) ++ " " ++ (show unit) ++ "s"


format :: Time -> String
format [] = "now"
format [x] = formatUnit x
format [x, y] = (formatUnit x) ++ " and " ++ (formatUnit y)
format (x:xs) = (formatUnit x) ++ ", " ++ (format xs)

formatDuration :: (Integral i) => i -> String
formatDuration n = format . reverse $ result where
    result = getDate (fromIntegral n) Second
