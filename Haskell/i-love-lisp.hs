{-# LANGUAGE FlexibleInstances #-}
module LispLovesMe where

import Text.ParserCombinators.Parsec
import Control.Applicative (liftA2)
import Data.List

data AST = I32 Int
          | Sym String
          | Nul
          | Err
          | Lst [AST]
          | Boo Bool
          | Nod AST [AST]
          deriving (Eq, Show)

class Value a where
    getValue :: AST -> Maybe a

instance Value (Int) where
    getValue (I32 n) = Just n
    getValue _ = Nothing
instance Value (Bool) where
    getValue (Boo n) = Just n
    getValue _ = Nothing
instance Value ([AST]) where
    getValue (Lst ast) = Just ast
    getValue _ = Nothing

-- Parse

whiteSpaces :: [Char]
whiteSpaces = " ,\r\n\t"

pWhiteSpaces :: CharParser st [Char]
pWhiteSpaces = many . oneOf $ whiteSpaces

pI32 :: CharParser st AST
pI32 = I32 . read <$> many1 digit

pSym :: CharParser st AST
pSym = Sym <$> liftA2 (:) (noneOf (" ,\n\t\r()" ++ [ '0' .. '9' ])) (many . noneOf $ " ,\n\t\r()")

pNul :: CharParser st AST
pNul = Nul <$ (string "null" <|> string "()")

pBoo :: CharParser st AST
pBoo = (Boo True <$ string "true") <|> (Boo False <$ string "false")

pNod :: CharParser st AST
pNod = Nod <$> ((char '(') *> pExpr) <*> (many (try pExpr) <* pWhiteSpaces <* (char ')'))

pExpr :: CharParser st AST
pExpr = pWhiteSpaces *>
        (try pNul <|> pNod <|> try pBoo <|> pI32 <|> pSym)

pLisp :: CharParser st AST
pLisp = pExpr <* pWhiteSpaces <* eof

lispParse :: String -> Maybe AST
lispParse s = rightToMaybe $ parse pLisp "(unknown)" s where
    rightToMaybe = either (const Nothing) Just

-- Eval

mapMaybe :: (AST -> Maybe a) -> [AST] -> Maybe [a]
mapMaybe f [] = Just []
mapMaybe f (x:xs) = do
    n <- f x
    ns <- mapMaybe f xs
    return $ n : ns

getValueList :: Value a => [AST] -> Maybe [a]
getValueList = mapMaybe getValue

maybeToAst :: Maybe AST -> AST
maybeToAst m = case m of
    Nothing  -> Err
    Just ast -> ast

lengthInt :: [AST] -> Int
lengthInt = length

preludeFunctionsMaybe :: [(String, [AST] -> Maybe AST)]
preludeFunctionsMaybe =
    [ ("+", \xs -> I32 . sum <$> getValueList xs)
    , ("*", \xs -> I32 . product <$> getValueList xs)
    , ("-", \xs -> I32 . subL <$> getValueList xs)
    , ("/", \xs -> getValueList xs >>= divL)
    , ("^", \xs -> I32 <$> (getValueList xs >>= ap2 (^)))
    , (">", \xs -> Boo <$> (getValueList xs >>= ap2 ((>) :: Int -> Int -> Bool)))
    , ("<", \xs -> Boo <$> (getValueList xs >>= ap2 ((<) :: Int -> Int -> Bool)))
    , ("==", \xs -> Boo <$> (getValueList xs >>= ap2 ((==) :: Int -> Int -> Bool)))
    , (">=", \xs -> Boo <$> (getValueList xs >>= ap2 ((>=) :: Int -> Int -> Bool)))
    , ("<=", \xs -> Boo <$> (getValueList xs >>= ap2 ((<=) :: Int -> Int -> Bool)))
    , ("!=", \xs -> Boo <$> (getValueList xs >>= ap2 ((/=) :: Int -> Int -> Bool)))
    , ("!", \xs -> Boo <$> (getValueList xs >>= ap1 not))
    , ("list", Just . Lst)
    , ("size", \xs -> I32 . length . head <$> ((getValueList xs) :: Maybe [[AST]]))
    , ("reverse", \xs -> Lst . reverse . head <$> ((getValueList xs) :: Maybe [[AST]]))
    , ("..", \xs -> Lst . map I32 <$> (getValueList xs >>= ap2 range))
    , ("if", condition)
    ]
    where
    subL (x:xs) = x - (sum xs)
    divL [x] = Just (I32 x)
    divL (x:0:_) = Nothing
    divL (x:y:xs) = divL $ (x `div` y) : xs
    range x y = [x .. y]
    condition [c, x] = condition [c, x, Nul]
    condition [Boo boo, x, y] = Just $ if boo then x else y
    condition _ = Nothing
    ap1 f [x] = Just (f x)
    ap1 _ _   = Nothing
    ap2 f [x, y] = Just (f x y)
    ap2 _ _      = Nothing

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions = map (\(k, f) -> (k, \xs -> maybeToAst . f $ xs))preludeFunctionsMaybe

eval :: AST -> AST
eval ast@(Nod (Sym sym) xs) =
    case lookup sym preludeFunctions of
        Just f  -> f . map eval $ xs
        Nothing -> Err
eval ast = ast

lispEval :: String -> Maybe AST
lispEval s = do
    ast <- lispParse s
    return $ eval ast

-- Pretty

pretty :: AST -> String
pretty (I32 n) = show n
pretty (Sym s) = s
pretty (Boo b) = if b then "true" else "false"
pretty Nul     = "null"
pretty (Nod x xs) = "(" ++ (intercalate " " . map pretty $ (x:xs)) ++ ")"

lispPretty :: String -> Maybe String
lispPretty s = do
    ast <- lispParse s
    return $ pretty ast
