module Kata.Cut.Cake (cut) where

import Data.Tuple
import Data.List
import Control.Monad
import Data.Monoid

type Point = (Int, Int)
data Rect = Rect
    { pos  :: Point
    , area :: [[Char]]
    } deriving (Eq)

instance Show Rect where
    show r = unlines $ ["", show . pos $ r] ++ (area r)

instance Ord Rect where
    compare r1 r2 = pos r1 `compare` pos r2 <> width r2 `compare` width r1

format :: String -> Rect
format s = Rect { pos = (0, 0), area = lines s }

width :: Rect -> Int
width r = length . head $ area r

height :: Rect -> Int
height r = length $ area r

widthIf :: Bool -> Rect -> Int
widthIf False = width
widthIf True = height

areaSize :: Rect -> Int
areaSize r = width r * height r

transposeRect :: Rect -> Rect
transposeRect r = Rect { pos = swap $ pos r, area = transpose $ area r}

splitX :: Int -> Rect -> (Rect, Rect)
splitX num r =
    let (x, y) = pos r in
    ((r { area = take num $ area r }), (r { pos = (x + num, y), area = drop num $ area r }))

splitY :: Int -> Rect -> (Rect, Rect)
splitY num r = mapTupple2 transposeRect . splitX num . transposeRect $ r where
    mapTupple2 f (x, y) = (f x, f y)

splitIf :: Bool -> Int -> Rect -> (Rect, Rect)
splitIf False = splitX
splitIf True = splitY

reverseIf :: Bool -> [a] -> [a]
reverseIf True = reverse
reverseIf False = id

sumRaisins :: Rect -> Int
sumRaisins r = length [ x | xs <- area r, x <- xs, x == 'o']

go :: Int -> Rect -> [[Rect]]
go num r | num /= sumRaisins r = []
go num r | areaSize r `mod` num /= 0 = []
go 1 r = [[r]]
go num r = do
    trans <- [False, True]
    let
        size = areaSize r
        w = widthIf trans r
        xs = [ x | x <- [1 .. num - 1] , size * x `div` num `mod` w == 0 ]
    x <- xs
    let h = size * x `div` num `div` w
    let (r1, r2) = splitIf trans h r
    result1 <- go x r1
    result2 <- go (num - x) r2
    return $ result1 ++ result2

cut :: String -> [String]
cut cake =
    let r = format cake
        rs = go (sumRaisins r) r
    in if null rs then [] else map (unlines . area) . head . sort . map sort $ rs
