module Haskell.SylarDoom.Smallfuck where

not' :: Char -> Char
not' '0' = '1'
not' '1' = '0'

run :: String -> String -> String -> String -> String
run dx [] ls rs = (reverse ls) ++ rs;
run dx ('*':cx) ls (r:rs) = run ('*':dx) cx ls ((not' r):rs)
run dx ('>':cx) ls [r] = reverse(r:ls)
run dx ('>':cx) ls (r:rs) = run ('>':dx) cx (r:ls) rs
run dx ('<':cx) [] rs = rs
run dx ('<':cx) (l:ls) rs = run ('<':dx) cx ls (l:rs)
run dx ('[':cx) ls (r:rs) =
    if r == '0'
    then goPass 1 ('[':dx) cx ls (r:rs)
    else run ('[':dx) cx ls (r:rs)
run dx (']':cx) ls (r:rs) =
    if r /= '0'
    then goBack 1 dx (']':cx) ls (r:rs)
    else run (']':dx) cx ls (r:rs)
run dx ( c :cx) ls rs = run (c:dx) cx ls rs

goPass :: Int -> String -> String -> String -> String -> String
goPass deep dx [] ls rs = run [] [] ls rs
goPass 1 dx (']':cx) ls rs = run (']':dx) cx ls rs
goPass deep dx (']':cx) ls rs = goPass (deep - 1) (']':dx) cx ls rs
goPass deep dx ('[':cx) ls rs = goPass (deep + 1) ('[':dx) cx ls rs
goPass deep dx (c:cx) ls rs = goPass deep (c:dx) cx ls rs

goBack :: Int -> String -> String -> String -> String -> String
-- goBack a b c d e = error . show $ b
goBack deep [] cx ls rs = run [] [] ls rs
goBack 1 ('[':dx) cx ls rs = run ('[':dx) cx ls rs
goBack deep ('[':dx) cx ls rs = goBack (deep - 1) dx ('[':cx) ls rs
goBack deep (']':dx) cx ls rs = goBack (deep + 1) dx (']':cx) ls rs
goBack deep (d:dx) cx ls rs = goBack deep dx (d:cx) ls rs

interpreter :: String -> String -> String
interpreter code [] = []
interpreter [] tape = tape
interpreter code tape = run [] code [] tape
