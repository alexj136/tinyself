module Syntax where

import Name
import qualified Data.List as L
import qualified Data.Map  as M

type Function = (Name, Term)

data Term
    = Lit (M.Map Name Function)
    | Var Name
    | Call Term Name
    | Assign Term Name Function
    deriving (Eq, Ord)

instance Show Term where
    show tm = case tm of
        Lit mets            -> "[ " ++ (concat (L.intersperse " , " (
            map (\(l, (s, m)) -> show l ++ ":$(" ++ show s ++ ')' : show m)
                (M.toList mets)
            ))) ++ " ]"
        Var name            -> show name
        Call obj name       -> show obj ++ '.' : show name
        Assign obj name (s, bdy) -> show obj ++ " <- " ++ show name ++
            ":$(" ++ show s ++ ")" ++ show bdy

toStringTerm :: NameEnv Term -> String
toStringTerm env@(NameEnv nameMap nextName term) = case term of
    Lit mets            -> "[ " ++ (concat (L.intersperse " , " (
        map (\(l, (s, m)) -> query l env ++
            ":$(" ++ query s env ++ ')' :
                (toStringTerm (NameEnv nameMap nextName m)))
            (M.toList mets)
        ))) ++ " ]"
    Var name            -> query name env
    Call obj name       -> (toStringTerm (NameEnv nameMap nextName obj)) ++
        '.' : query name env
    Assign obj name (s, bdy) -> (toStringTerm (NameEnv nameMap nextName obj)) ++
        " <- " ++ query name env ++ ":$(" ++ query s env ++ ")" ++
        (toStringTerm (NameEnv nameMap nextName bdy))
