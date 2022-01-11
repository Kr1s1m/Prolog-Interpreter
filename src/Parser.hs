module Parser where
import Control.Monad
import Control.Applicative (Alternative((<|>), empty, many, some))
import Data.Char(isAsciiLower, isAsciiUpper, isDigit)

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

ident :: Parser Ident
ident = do
    c  <- lowerCase
    cs <- many alphanumeric
    return $ Ident (c:cs)

    
var :: Parser Var 
var = do
    c  <- upperCase <|> char '_'
    cs <- many alphanumeric
    return $ Var (c:cs)



parse :: Parser a -> String -> Maybe (a, String)
parse = runParser