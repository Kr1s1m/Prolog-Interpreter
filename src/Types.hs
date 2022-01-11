module Types where

newtype Ident = Ident String deriving (Eq, Show, Ord)
newtype Var = Var String deriving (Eq, Show, Ord)

data Comp = Comp Ident [Term] deriving (Eq, Show)

data Term = TIdent Ident | TVar Var | TComp Comp deriving (Eq, Show)

newtype Atom = Atom Comp deriving (Eq, Show)

data Clause = Fact Atom | Rule Atom [Atom] deriving (Eq, Show)

type Program = [Clause]

newtype Query = Query Term deriving (Eq, Show)

type Goal = [Query]



getHead :: Clause -> Atom
getHead (Fact head) = head
getHead (Rule head _) = head