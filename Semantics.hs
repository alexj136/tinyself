module Semantics where

import qualified Data.Map as M
import qualified Data.Set as S

import Syntax

eval :: Term -> Term
eval tm = case tm of
    Lit mets            -> tm
    Var name            -> tm
    Call obj name       ->
        case obj of
            Lit mets    -> replace obj (mets M.! name)
            Var name    -> tm
            _           -> eval (Call (eval obj) name)
    Assign obj name fn  ->
        case obj of
            Lit mets    -> Lit (M.insert name fn mets)
            Var name    -> tm
            _           -> eval (Assign (eval obj) name fn)

replace :: Term -> Function -> Term
replace selfTerm (selfName, body) = case body of --{ _ -> error "nyi" }
    Lit mets                -> Lit (fmap (\(l, (s, m)) -> ...) mets)
    Var name                -> if name == selfName then selfTerm else body
    Call obj name           -> Call (repl obj) name
    Assign obj name (s, b)  -> Assign (repl obj) name (...)
    where
        repl :: Term -> Term
        repl bdoy' = replace selfTerm (selfName, body')

-- Generate a name that is not free in a given term
freshFor :: Term -> Name
freshFor = findFresh . freeVars

-- Enumerate the free variable names in a term
freeVars :: Term -> S.Set Name
freeVars tm = case tm of
    Lit mets            -> S.unions (map freeVarsFn (M.elems mets))
    Var name            -> S.singleton name
    Call obj name       -> freeVars obj
    Assign obj name fn  -> S.union (freeVars obj) (freeVarsFn fn)
    where
        freeVarsFn :: Function -> S.Set Name
        freeVarsFn (self, body) = S.delete self (freeVars body)

-- Generate a name that does not belong to a particular set of names (i.e. a set
-- of free names from a term)
findFresh :: S.Set Name -> Name
findFresh names = findFresh' names (S.findMin names)
    where
    -- Test if a name is in the set. If it isn't return it, otherwise add an
    -- apostrophe to it and try again
    findFresh' :: S.Set Name -> Name -> Name
    findFresh' names (Name lastTry)
        | otherwise                     = Name lastTry
        | S.member (Name lastTry) names =
            findFresh' names (Name (lastTry ++ "'"))
