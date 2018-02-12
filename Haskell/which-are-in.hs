module Codewars.Kata.Which where

import Data.List

-- Sorry for the name of the function.
inArray :: [String] -> [String] -> [String]
inArray a1 a2 = nub . sort $ [s | s <- a1, any (isInfixOf s) a2]
