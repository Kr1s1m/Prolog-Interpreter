module Parser where

import Control.Applicative (Alternative((<|>), empty, many, some))
import Data.Char(isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Maybe ( fromMaybe )


import Types
    ( Funct(Funct),
      Ident,
      Program,
      Term(Comp, Const, Var),
      VarName,
      Rule(Rule),
      Command(NoAction, AddRule, Query, ShowAllRules, Help, Quit, CmdError) )


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
    p <|> q = Parser $ \s -> runParser p s <|> runParser q s

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

chars :: [Char] -> Parser Char
chars cs = charPred (`elem` cs)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
    c' <- char c
    cs' <- string cs
    return (c':cs')

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

newline :: Parser String
newline = many $ charPred (== '\n')

strip :: Parser a -> Parser a
strip p = do
    whitespace <|> newline
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
    return (c:cs)

varname :: Parser VarName
varname = do
    c  <- upperCase <|> char '_'
    cs <- many alphanumeric
    return (c:cs)

const' :: Parser Term
const' = do
    Const <$> ident'

var :: Parser Term
var = do
    v <- varname
    return $ Var (v, 0)

comp :: Parser Term
comp = do
    id <- strip ident'
    char '('
    args <- arguments $ strip term
    strip $ char ')'
    return $ Comp (Funct id (length args)) args

term :: Parser Term
term = comp <|> const' <|> var

fact :: Parser Rule
fact = do
    head <- comp
    strip $ char '.'
    return $ Rule head []

rule :: Parser Rule
rule = do
    head <- comp
    strip $ char ':'
    char '-'
    body <- arguments comp
    strip $ char '.'
    return $ Rule head body

clause :: Parser Rule
clause = fact <|> rule

program :: Parser Program
program = do
    rules <- many clause
    whitespace <|> newline
    return rules

query :: Parser Command
query = do
    strip $ char '?' <|> char ':'
    char '-'
    goals <- arguments $ strip term
    strip $ char '.'
    return $ Query goals

addRule :: Parser Command
addRule = do
    AddRule <$> clause

showAllRules :: Parser Command
showAllRules = do
    strip $ char '?'
    char '?'
    return ShowAllRules


quit :: Parser Command 
quit = do
    strip $ char ':'
    char 'q'
    return Quit

help :: Parser Command
help = do
    strip $ char ':'
    string "help"
    return Help

        
noAction :: Parser Command
noAction = do
    whitespace <|> newline
    string ""
    return NoAction

command' :: Parser Command 
command' = query <|> addRule <|> showAllRules  <|>  help <|> quit <|> noAction

command :: Parser Command
command = do
    cmd <- command'
    whitespace <|> newline
    return cmd


parse :: Parser a -> String -> Maybe (a, String)
parse = runParser

parseCheck :: Maybe (a, String) -> Maybe a
parseCheck (Just (a, "")) = Just a
parseCheck _ = Nothing

parseCommand :: String -> Command
parseCommand s = fromMaybe CmdError $ parseCheck (parse command s)

parseProgram :: String -> Program
parseProgram s = fromMaybe [] $ parseCheck (parse program s)