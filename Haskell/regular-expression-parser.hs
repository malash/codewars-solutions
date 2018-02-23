module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where
import Text.ParserCombinators.Parsec

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
            deriving (Show, Eq)

listToStr :: [RegExp] -> RegExp
listToStr [x] = x
listToStr xs = Str xs

p_term :: GenParser Char st RegExp
p_term = p_brackets <|> p_any <|> p_normal where
    p_brackets = between (char '(') (char ')') p_or
    p_any      = Any    <$  char '.'
    p_normal   = Normal <$> noneOf "()*|."

p_many :: GenParser Char st RegExp
p_many = try (ZeroOrMore <$> p_term <* char '*') <|> p_term

p_str :: GenParser Char st RegExp
p_str = listToStr <$> many1 p_many

p_or :: GenParser Char st RegExp
p_or = try (Or <$> p_str <* char '|' <*> p_str) <|> p_str

p_regexp :: GenParser Char st RegExp
p_regexp = p_or <* eof

parseRegExp :: String -> Maybe RegExp
parseRegExp s = rightToMaybe $ parse p_regexp "(unknown)" s where
    rightToMaybe = either (const Nothing) Just
