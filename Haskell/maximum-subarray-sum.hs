module MaxSequence where

import Data.List

-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence = maximum . map sum . concat . map inits . tails
