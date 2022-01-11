module Parser where
import Control.Monad(void)
import Control.Applicative (Alternative((<|>), empty, many, some))
import Data.Char(isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Text.Parsec(try)

import Types


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f m = m >>= \a -> return (f a)

instance Applicative Parser where
    pure = return
    df <*> dx = df >>= \f -> fmap f dx

instance Monad Parser where
    return a = Parser $ \s -> Just (a, s)
    m >>= f = Parser $ \s -> do
        (a, s') <- runParser m s
        runParser (f a) s'

instance Alternative Parser where
    empty = reject
    p <|> q = Parser $ \s -> parse p s <|> parse q s


reject :: Parser a
reject = Parser $ const Nothing


getC :: Parser Char
getC = Parser getC'
    where
        getC' ""     = Nothing
        getC' (c:cs) = Just (c, cs)

charPred :: (Char -> Bool) -> Parser Char
charPred pred = do
    c <- getC
    if pred c then return c else reject

char :: Char -> Parser Char
char c = charPred (== c)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
    void $ char c
    void $ string cs
    return (c:cs)

chars :: [Char] -> Parser Char
chars cs = charPred (`elem` cs)

wild :: Parser ()
wild = void getC

numeric :: Parser Char
numeric = charPred isDigit

lowerCase :: Parser Char
lowerCase  = charPred isAsciiLower

upperCase :: Parser Char
upperCase = charPred isAsciiUpper

alphabetic :: Parser Char
alphabetic = lowerCase <|> upperCase

alphanumeric :: Parser Char
alphanumeric = alphabetic <|> numeric

whitespace :: Parser String
whitespace = many $ charPred isSpace

strip :: Parser a -> Parser a
strip p = do
    whitespace
    p

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do
    a <- p
    as <- many $ do {sep; p}
    return (a:as)

arguments' :: Parser a -> Parser [a]
arguments' p = p `sepBy` strip (char ',')

arguments :: Parser a -> Parser [a]
arguments p = arguments' p <|> return []

ident' :: Parser Ident
ident' = do
    c  <- lowerCase
    cs <- many alphanumeric
    return $ Ident (c:cs)

ident :: Parser Term
ident = do
    TIdent <$> ident'


var' :: Parser Var
var' = do
    c  <- upperCase <|> char '_'
    cs <- many alphanumeric
    return $ Var (c:cs)

var :: Parser Term
var = do
    TVar <$> var'


comp' :: Parser Comp
comp' = do   
    id <- strip ident'
    char '('
    args <- arguments $ strip term
    strip $ char ')'
    return $ Comp id args

comp :: Parser Term
comp = do
    TComp <$> comp'

term :: Parser Term
term = comp <|> ident <|> var

atom :: Parser Atom
atom = do
    Atom <$> comp'

fact :: Parser Clause
fact = do
    a <- atom
    strip $ char '.'
    return $ Fact a

rule :: Parser Clause
rule = do
    head <- atom
    strip $ char ':'
    char '-'
    body <- arguments $ strip atom
    strip $ char '.'
    return $ Rule head body

clause :: Parser Clause 
clause = fact <|> rule

program :: Parser Program
program = do
    cls <- many clause
    whitespace
    return cls

parse :: Parser a -> String -> Maybe (a, String)
parse = runParser