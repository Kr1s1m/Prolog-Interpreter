module Parser where

import Control.Applicative (Alternative((<|>), empty, many, some))
import Data.Char(isAsciiLower, isAsciiUpper, isDigit, isSpace)

import Types
    ( KnowledgeBase,
      Program,
      Clause(Fact, Rule, head),
      Term(Comp, Const, Var, funct),
      Funct(Funct),
      VarName,
      Ident 
    )

import Data.Map as Map (empty, insertWith)



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
    Var <$> varname


comp :: Parser Term
comp = do
    id <- strip ident'
    char '('
    args <- arguments $ strip term
    strip $ char ')'
    return $ Comp (Funct id (length args)) args


term :: Parser Term
term = comp <|> const' <|> var


fact :: Parser Clause
fact = do
    a <- comp
    strip $ char '.'
    return $ Fact a

rule :: Parser Clause
rule = do
    head <- comp
    strip $ char ':'
    char '-'
    body <- arguments comp
    strip $ char '.'
    return $ Rule head body

clause :: Parser Clause
clause = fact <|> rule

program :: Parser Program
program = do
    cls <- many clause
    whitespace <|> newline
    return cls

parse' :: Parser a -> String -> Maybe (a, String)
parse' = runParser


parseCheck :: Maybe (a, String) -> Maybe a
parseCheck (Just (a, "")) = Just a
parseCheck _ = Nothing

parse :: String -> Maybe Program
parse s = parseCheck parseResult
    where parseResult = parse' program s


createKnowledgeBase :: Maybe Program -> Maybe KnowledgeBase
createKnowledgeBase program = case program of
    Nothing -> Nothing
    Just prog -> Just $ createKnowledgeBase' prog Map.empty

    where
        createKnowledgeBase' :: Program -> KnowledgeBase -> KnowledgeBase
        createKnowledgeBase' prog kbase = case prog of
            [] -> kbase
            (cl:cls) -> createKnowledgeBase' cls (addClause cl kbase)

        addClause :: Clause -> KnowledgeBase -> KnowledgeBase
        addClause cl kbase = Map.insertWith (flip (++)) (funct (Types.head cl)) [cl] kbase