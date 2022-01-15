module Types where

import Data.Map as Map
import Data.List(intercalate)


type Ident = String
type VarName = String

data Funct = Funct {ident :: Ident, arity :: Int} deriving (Eq)

instance Show Funct where
    show f = ident f

instance Ord Funct where
    compare (Funct ide ari) (Funct ide' ari') = if ide == ide' then compare ari ari' else compare ide ide'
     

data Term = Const Ident | Var VarName | Comp {funct :: Funct, args :: [Term]} deriving (Eq)

instance Show Term where
    show (Const ident) = ident
    show (Var varname) = varname
    show (Comp f args) = show f ++ "(" ++ intercalate ", " (Prelude.map show args) ++ ")"


data Clause = Fact {head :: Term} | Rule {head :: Term, body :: [Term]} deriving (Eq, Show)

type Program = [Clause]

type KnowledgeBase = Map Funct [Clause]

type Goal = [Term]