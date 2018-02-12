module Spiral where

data Direction = T | R | B | L deriving (Enum, Eq, Show)

type Pointer = (Int, Int)
type Map = [[Int]]

next :: Direction -> Direction
next L = T
next d = succ d

move :: Direction -> Pointer -> Pointer
move T (x, y) = (x - 1, y)
move R (x, y) = (x, y + 1)
move B (x, y) = (x + 1, y)
move L (x, y) = (x, y - 1)

validateOne :: Int -> Int -> Bool
validateOne n x
  | 0 <= x && x < n = True
  | otherwise = False

validate :: Int -> Pointer -> Bool
validate n (x, y) = all (validateOne n) [x, y]

get :: Int -> Map -> Pointer -> Maybe Int
get n map pointer@(x, y) =
    if validate n pointer
    then Just $ map !! x !! y
    else Nothing

replace :: Int -> a -> [a] -> [a]
replace x v l = xs ++ [v] ++ ys  where (xs, _:ys) = splitAt x l

set :: Pointer -> Int -> Map -> Map
set (x, y) v map = replace x (replace y v (map !! x)) map

go :: Int -> Pointer -> Direction -> Map -> Map
go n pointer d map =
    let map'    = set pointer 1 map
        step    = move d pointer
        step'   = move d step
        nextD   = next d
        nextD'  = next nextD
        turn    = move nextD pointer
        turn'   = move nextD' turn
        canTurn = (get n map turn, get n map turn') == (Just 0, Just 0)
    in  case (get n map step, get n map step') of
            (Just 1, _      ) -> map
            (Just 0, Just 1 ) -> if canTurn then go n turn (next d) map' else map'
            (Nothing, _     ) -> if canTurn then go n turn (next d) map' else map'
            otherwise         -> go n (step) d map'

spiralize :: Int -> Map
spiralize n = go n (0, 0) R (replicate n . replicate n $ 0)
