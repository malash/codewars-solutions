module Kata.DecodeMorseAdvanced where

import Kata.DecodeMorseAdvanced.Preload
import Data.Char
import Data.List (find, dropWhile, dropWhileEnd)
import Data.Map.Strict ((!), fromList, toList, findWithDefault)
import Data.Foldable (asum)
import Data.List.Split (wordsBy)
import Text.ParserCombinators.Parsec

-- common

trimWith :: (Char -> Bool) -> String -> String
trimWith f = dropWhileEnd f . dropWhile f

-- decodeBits

infixr 6 ***
(***) :: a -> Int -> [a]
a *** num = take num . repeat $ a

discerningTimeUnit :: String -> Int
discerningTimeUnit s = minimum $ [length s] ++ (map length . wordsBy (=='0') $ s) ++ (map length . wordsBy (=='1') $ s)

pBit :: Int -> CharParser st String
pBit n =
        "." <$ try (string $ '1' *** n ++ '0' *** n)
    <|> " " <$ try (string $ '0' *** (n * 2))
    <|> "-" <$ try (string $ '1' *** (n * 3) ++ '0' *** n)

pBits :: Int -> CharParser st String
pBits n = concat <$> many (pBit n) <* eof

decodeBits :: String -> String
decodeBits s =
    let trimed = trimWith (=='0') s
        n = discerningTimeUnit trimed
    in  case parse (pBits n) "(bits)" ((++ '0' *** n) $ trimed) of
            Right r -> r
            Left  _ -> show $ map length . wordsBy (=='1') $ s

-- decodeMorse

pWord :: CharParser st String
pWord = " " <$ char ' '
    <|> flip (findWithDefault "") morseCodes <$> many (satisfy (not . isSpace))

pMorse :: CharParser st String
pMorse = (concat <$> sepBy pWord space)

decodeMorse :: String -> String
decodeMorse s = case parse pMorse "(morse)" (trimWith isSpace s) of
    Right r -> r
    Left  _ -> s
