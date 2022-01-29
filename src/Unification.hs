module Unification where

import Types ( Term(Var, Const, Comp), Substitution, true )


unify :: Term -> Term -> Maybe Substitution
unify (Const c1) (Const c2) = if c1 == c2 then Just true else Nothing
unify (Var v1) (Var v2)     = if v1 == v2 then Just true else Just [(Var v1, Var v2)]
unify (Var v) t = Just [(Var v, t)] 
unify t (Var v) = Just [(Var v, t)]
unify (Comp f1 args1) (Comp f2 args2) = if f1 == f2 then unify' args1 args2 else Nothing
unify _ _ = Nothing


unify' :: [Term] -> [Term] -> Maybe Substitution
unify' [] [] = Just true
unify' _ [] = Nothing
unify' [] _ = Nothing
unify' (t:ts) (u:us) = do
    s  <- unify t u
    s' <- unify' (applySubstitution s ts) (applySubstitution s us)
    return (s ++ s')


applySubstitution :: Substitution -> [Term] -> [Term]
applySubstitution s = map (applySubstitution' s)

applySubstitution' :: Substitution -> Term -> Term
applySubstitution' [] t = t
applySubstitution' ((Var v1, t):ss) (Var v2)
    | v1 == v2  = applySubstitution' ss t
    | otherwise = applySubstitution' ss (Var v2)
applySubstitution' s (Comp f args) = Comp f $ applySubstitution s args
applySubstitution' _ t = t