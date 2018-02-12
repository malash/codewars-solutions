module RPN where

run :: [Double] -> [String] -> Double
run (s:ss) [] = s
run (s1:s2:ss) ("+":xs) = run ((s2 + s1):ss) xs
run (s1:s2:ss) ("-":xs) = run ((s2 - s1):ss) xs
run (s1:s2:ss) ("*":xs) = run ((s2 * s1):ss) xs
run (s1:s2:ss) ("/":xs) = run ((s2 / s1):ss) xs
run ss (x:xs) = run ((read x) : ss) xs

calc :: String -> Double
calc [] = 0
calc xs = run [] . words $ xs
