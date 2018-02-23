module Codewars.Kata.DecodeMorse (decodeMorse) where

import Codewars.Kata.DecodeMorse.Preload (morseCodes)
import Data.Char
import Data.List (dropWhile, dropWhileEnd)
import Data.Map.Strict ((!), fromList, toList, findWithDefault)
import Text.ParserCombinators.Parsec

pWord :: CharParser st String
pWord = " " <$ char ' '
    <|> flip (findWithDefault "") morseCodes <$> many (satisfy (not . isSpace))

pMorse :: CharParser st String
pMorse = (concat <$> sepBy pWord space)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

decodeMorse :: String -> String
decodeMorse s = case parse pMorse "(unknown)" (trim s) of
    Right r -> r
    Left _ -> ""
