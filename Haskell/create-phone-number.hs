module CreatePhoneNumber where

createPhoneNumber :: [Int] -> String
createPhoneNumber xs = '(' : a : b : c : ')' : ' ' : d : e : f : '-' : g : h : i : j where
  (a:b:c:d:e:f:g:h:i:j) = map (head . show) xs
