module Unification where
import Types

import Data.List(zipWith)


unify :: Term -> Term -> Substitutions
unify (Const c1) (Const c2) = if c1 == c2 then Just [] else Nothing
unify (Var v) (Const c) = Just [(Var v, Const c)]
unify (Const c) (Var v) = Just [(Var v, Const c)]
unify (Var v1) (Var v2) = if v1 == v2 then Just [] else Just [(Var v1, Var v2), (Var v2, Var v1)]
unify (Var v) (Comp f args) = Just [(Var v, Comp f args)]
unify (Comp f args) (Var v)  = Just [(Var v, Comp f args)]
unify (Comp f1 args1) (Comp f2 args2) = if f1 == f2 then checkForDupVar $ joinUnified (zipWith unify args1 args2) [] else Nothing   
    where 
          joinUnified [] res = Just res
          joinUnified (u:us) res = case u of
              Just [] -> joinUnified us res
              Just sub -> joinUnified us (sub ++ res)
              Nothing -> Nothing

          checkForDupVar joined = case joined of
              Nothing -> Nothing 
              Just j -> if any (\(x, y) -> isVar x && not ( isVar y ) && any (\(z, t) -> z == x && not ( isVar t) && y /= t) j )  j
                        then Nothing 
                        else Just j
 
          isVar (Var _) = True
          isVar _ = False

unify _ _ = Nothing