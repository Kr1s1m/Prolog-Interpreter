module Types where

import Data.Map.Lazy as Map
import Data.List(intercalate)

type Ident = String
type VarName = String

data Funct = Funct {ident :: Ident, arity :: Int} deriving (Eq)

instance Show Funct where
    show f = ident f

instance Ord Funct where
    compare (Funct id ari) (Funct id' ari') = if id == id' then compare ari ari' else compare id id'
     

data Term = Const Ident | Var VarName | Comp {funct :: Funct, args :: [Term]} deriving (Eq)

instance Show Term where
    show (Const ident) = ident
    show (Var varname) = varname
    show (Comp f args) = show f ++ "(" ++ intercalate ", " (Prelude.map show args) ++ ")"


type Substitutions = Maybe [(Term, Term)]

data Clause = Fact {head :: Term} | Rule {head :: Term, body :: [Term]} deriving (Eq, Show)

type Program = [Clause]

type KnowledgeBase = Map Funct [Clause]

type Goal = [Term]