module Semantics where

import qualified Data.Map as M
import qualified Data.Set as S

import Syntax

interpret :: Term -> Maybe (M.Map Name Function)
interpret term = case term of
    Lit    funcs  -> return funcs
    Var    n      -> Nothing
    Call   tm l   -> do
        funcs <- interpret tm
        lfunc <- M.lookup l funcs
        interpret $ subTerm (snd lfunc) (fst lfunc) (Lit funcs)
    Assign tm l f -> do
        funcs <- interpret tm
        return $ M.insert l f funcs

subTerm :: Term -> Name -> Term -> Term
subTerm within from to = case within of
    Lit    funcs  -> Lit $ M.map (\f -> subFunction f from to) funcs
    Var    n      -> if n == from then to else within
    Call   tm l   -> Call (subTerm tm from to) l
    Assign tm l f -> Assign (subTerm tm from to) l (subFunction f from to)

subFunction :: Function -> Name -> Term -> Function
subFunction (s, b) from to = (s', b')
    where
        s' :: Name
        s' = next $ S.findMax $ S.unions
            [ freesFunction (s, b)
            , freesTerm to
            , S.singleton from
            ]
        b' :: Term
        b' = subTerm (subTerm b s (Var s')) from to

freesTerm :: Term -> S.Set Name
freesTerm term = case term of
    Lit    funcs  -> S.unions $ map freesFunction $ M.elems funcs
    Var    n      -> S.singleton n
    Call   tm l   -> freesTerm tm
    Assign tm l f -> S.union (freesTerm tm) (freesFunction f)

freesFunction :: Function -> S.Set Name
freesFunction (s, b) = S.delete s (freesTerm b)
