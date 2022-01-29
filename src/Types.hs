module Types where

import Data.List(intercalate)

type Ident = String
type VarName = String

data Funct = Funct {ident :: Ident, arity :: Int} deriving (Eq)

instance Show Funct where
    show f = ident f

instance Ord Funct where
    compare (Funct id ari) (Funct id' ari')
        | idOrd == EQ = compare ari ari'
        | otherwise = idOrd
        where idOrd = compare id id'


data Term = Const Ident | Var (VarName, Int) | Comp {funct :: Funct, args :: [Term]} deriving (Eq)

instance Show Term where
    show (Const ident) = ident
    show (Var (varname, 0)) = varname
    show (Var (varname, n)) = varname ++ "_" ++ show n
    show (Comp f args) = show f ++ "(" ++ intercalate ", " (map show args) ++ ")"


data Rule = Rule {rhead :: Term, rbody :: [Term]} deriving (Eq)

instance Show Rule where
    show (Rule head []) = show head ++ "."
    show (Rule head body) = show head ++ ":-" ++ intercalate "," (map show body) ++ "."

type Program = [Rule]

type Substitution = [(Term, Term)]

true :: Substitution
true = []

showSubstitution :: Substitution -> String
showSubstitution [] = "true."
showSubstitution ss = intercalate ", " (map showSubstitution' ss)

showSubstitution' :: (Term, Term) -> String
showSubstitution' (v@(Var _), t) = show v ++ " = " ++ show t
showSubstitution' (_, _) = ""

type Goals = [Term]

type Branch = [(Substitution, Goals)]

data Command = Help | NoAction | AddRule Rule | Query [Term] | ShowAllRules | Quit | CmdError deriving(Eq, Show)


getGoals :: Command -> [Term]
getGoals (Query q) = q
getGoals _ = []