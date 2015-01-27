module Semantics where

import qualified Data.Map  as M

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
