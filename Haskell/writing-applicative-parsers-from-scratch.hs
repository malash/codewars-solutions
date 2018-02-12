module ApplicativeParser where

import Data.Char
import Data.List
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f (P g) = P (map (\(s, a) -> (s, f a)) . g)

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
x <# p = (const x) <#> p

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P f where
    f [] = []
    f (x:xs) = [(xs, x) | p x]

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP char = predP (== char)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \xs -> [(xs, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
(P f) <@> (P g) = P $ \xs -> [(xs'', fn v) | (xs', fn) <- f xs, (xs'', v) <- g xs']

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = const <#> pa <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = flip const <#> pa <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
-- stringP :: String -> Parser String
-- stringP [] = P f where
--     f xs | null xs   = [("", "")]
--          | otherwise = []
-- stringP (x:xs) = (:) <#> charP x <@> stringP xs

stringP :: String -> Parser String
stringP = foldr (\ c -> (<@>) ((:) <#> charP c)) (inject [])

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P (const [])

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(<<>>) (P f) (P g) = P $ \xs -> concat [f xs, g xs]

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = some p <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> many p

run :: Parser a -> String -> [(String, a)]
run (P f) = f

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser (P f) xs = [v | (xs', v) <- f xs, null xs']

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p xs = case runParser p xs of
    [x] -> Just x
    _ -> Nothing

-- tupleP :: Parser a -> Parser (a, a)
-- tupleP p = (,) <#> (charP '(' @> p <@ charP ',') <@> (p <@ charP ')')

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
            | BinOpE BinOp Expr Expr
            | NegE Expr
            | ZeroE
            deriving (Eq, Show)
-- | Parse arithmetic expressions, with the following grammar:
--
-- expr         ::= const | binOpExpr | neg | zero
-- const        ::= int
-- binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
-- binOp        ::= '+' | '*'
-- neg          ::= '-' expr
-- zero         ::= 'z'
-- int          ::= digit +
-- digit        ::= '0' | ... | '9'
--

-- Parser Start

digitString2Int :: [Char] -> Int
digitString2Int cs = read cs

constEP :: Parser Expr
constEP = ConstE <#> (digitString2Int <#> many (predP isDigit))

negEP :: Parser Expr
negEP = NegE <#> (charP '-' @> exprP)

zeroEP :: Parser Expr
zeroEP = ZeroE <# charP 'z'

char2BinOp :: Char -> BinOp
char2BinOp '+' = AddBO
char2BinOp '*' = MulBO

binOpEP :: Parser Expr
binOpEP =
    flip BinOpE
    <#> (charP '('   @> exprP <@ charP ' ')
    <@> (char2BinOp <#> (charP '+' <<>> charP '*'))
    <@> (charP ' '   @> exprP <@ charP ')')

exprP :: Parser Expr
exprP = constEP <<>> negEP <<>> zeroEP <<>> binOpEP

parseExpr :: String -> Maybe Expr
parseExpr cs =
    case find (\(xs, expr) -> null xs) $ run (exprP) cs of
        Nothing -> Nothing
        Just (_, expr) -> Just expr

-- Parser End

evalExpr :: Expr -> Int
evalExpr (ConstE x) = x
evalExpr ZeroE = 0
evalExpr (NegE e) = -(evalExpr e)
evalExpr (BinOpE AddBO e1 e2) = (evalExpr e1) + (evalExpr e2)
evalExpr (BinOpE MulBO e1 e2) = (evalExpr e1) * (evalExpr e2)
