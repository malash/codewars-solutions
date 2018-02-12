module Codewars.Exercise.MergeChecker where

isMerge :: String -> String -> String -> Bool
isMerge "" "" "" = True
isMerge "" _ _ = False
isMerge z x "" = x == z
isMerge z "" y = y == z
isMerge (z:zs) (x:xs) (y:ys) =
    (x == z && isMerge zs xs (y:ys)) ||
    (y == z && isMerge zs (x:xs) ys)
