module MoleculeToAtoms where

import Data.Char
import Data.List
import Debug.Trace

data Token = Element String | Number Int | Begin | End | None deriving (Show, Eq)
type Counter = [(String, Int)]

flatten :: Foldable t => t [a] -> [a]
flatten = foldr1 (++)

parseToken :: String -> Either String (Token, String)
parseToken "" = Right (None, "")
parseToken css@(c:cs)
    | isUpper c      = let (element, rest) = span isLower cs in Right (Element (c : element), rest)
    | isDigit c      = let (number, rest) = span isDigit css in Right (Number . read $ number, rest)
    | c `elem` "([{" = Right (Begin, cs)
    | c `elem` ")]}" = Right (End, cs)
    | otherwise      = Left "Not a valid molecule"

parse :: [Counter] -> String -> (Either String ([Counter], String))
parse result "" = Right (result, "")
parse result formula = do
    (token, restFormula) <- parseToken formula
    case token of
        Element element -> parse ([(element, 1)] : result) restFormula
        Number number ->
            if null result
            then error "Number should follow element"
            else
                let (elements:restResult) = result
                    x = map (\(e, x) -> (e, x * number)) elements
                in parse (x : restResult) restFormula
        Begin -> do
            (block, restFormula') <- parse [] restFormula
            parse ((flatten block) : result) restFormula'
        End -> return (result, restFormula)

checkMatch :: String -> String -> Bool
checkMatch [] "" = True
checkMatch _  "" = False
checkMatch s ('(':xs) = (null s || head s == '[') && checkMatch ('(':s) xs
checkMatch s ('[':xs) = (null s || head s == '{') && checkMatch ('[':s) xs
checkMatch s ('{':xs) = null s                    && checkMatch ('{':s) xs
checkMatch s (')':xs) = head s == '('             && checkMatch (tail s) xs
checkMatch s (']':xs) = head s == '['             && checkMatch (tail s) xs
checkMatch s ('}':xs) = head s == '{'             && checkMatch (tail s) xs
checkMatch s ( _ :xs) = checkMatch s xs

merge :: Counter -> Counter
merge [] = []
merge [x] = [x]
merge (x@(xe, xn) : y@(ye, yn) : zs)
    | xe == ye = merge ((xe, xn + yn) : zs)
    | otherwise = x : (merge (y : zs))

parseMolecule ::  String -> Either String Counter
parseMolecule formula = do
    if not . checkMatch [] $ formula
    then Left "Mismatched parenthesis"
    else do
      (result, rest) <- parse [] formula
      return . merge . sort . flatten $ result
