module Solver where

import Types
    ( true,
      Program,
      Substitution,
      Term(Comp, Const, Var),
      Goals,
      Rule(Rule),
      Branch,
      showSubstitution )

import Unification ( unify, applySubstitution )
import Parser ( parseCommand, parseProgram )

import Data.Maybe ( maybeToList )

solve :: Program -> Goals -> [Substitution]
solve = dfs 1


dfs :: Int -> Program -> Goals -> [Substitution]
dfs _ _ [] = [true]
dfs n program goals = do
    (s, goals') <- branch (changeNamespace n program) goals
    solution    <- dfs (n + 1) program goals'
    return (s ++ solution)

branch :: Program -> Goals -> Branch
branch [] _ = []
branch _ [] = []
branch program (goal:goals) = do
    r@(Rule head body) <- program
    s                  <- maybeToList (unify goal head)
    return (s, applySubstitution s (body ++ goals))

changeNamespace :: Int -> Program -> Program
changeNamespace n = map renameRule
    where

        renameRule :: Rule -> Rule
        renameRule (Rule head body) = Rule (renameTerm head) (renameTerms body)

        renameTerm :: Term -> Term
        renameTerm c@(Const _)      = c
        renameTerm (Var (s, _))     = Var (s, n)
        renameTerm (Comp f args)    = Comp f (renameTerms args)

        renameTerms :: [Term] -> [Term]
        renameTerms = map renameTerm