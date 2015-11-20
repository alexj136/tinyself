module Syntax where

import Name
import qualified Data.List as L
import qualified Data.Map  as M

type Function = (Name, Term)
type FuncMap   = M.Map Name Function

data Term
    = Lit FuncMap
    | Var Name
    | Call Term Name
    | Assign Term Name Function
    | IntObj Int FuncMap
    deriving (Eq, Ord)

instance Show Term where
    show tm = case tm of
        Lit funcs           -> "[ " ++ (concat (L.intersperse " , " (
            map (\(l, f) -> show l ++ ":" ++ showFunction f) (M.toList funcs)
            ))) ++ " ]"
        Var name            -> show name
        Call obj name       -> show obj ++ '.' : show name
        Assign obj name (s, bdy) -> show obj ++ " <- " ++ show name ++
            ":$(" ++ show s ++ ")" ++ show bdy
        IntObj i funcs      -> show i ++ (concat (L.intersperse " <- " (
            map (\(l, f) -> show l ++ ":" ++ showFunction f) (M.toList funcs)
            )))

showFunction :: Function -> String
showFunction (nm, tm) = "$(" ++ show nm ++ ")" ++ show tm

toStringTerm :: NameEnv Term -> String
toStringTerm env@(NameEnv nameMap nextName term) = case term of
    Lit funcs                 -> "[ " ++ (concat (L.intersperse " , " (
        map (\(l, (s, m)) -> query l env ++
            ":$(" ++ query s env ++ ')' :
                (toStringTerm (NameEnv nameMap nextName m)))
            (M.toList funcs)
        ))) ++ " ]"
    Var name                 -> query name env
    Call obj name            -> (toStringTerm (NameEnv nameMap nextName obj)) ++
        '.' : query name env
    Assign obj name (s, bdy) -> (toStringTerm (NameEnv nameMap nextName obj)) ++
        " <- " ++ query name env ++ ":$(" ++ query s env ++ ")" ++
        (toStringTerm (NameEnv nameMap nextName bdy))
    IntObj i funcs           -> show i ++ (concat (L.intersperse " <- " (
        map (\(l, (s, m)) -> query l env ++
            ":$(" ++ query s env ++ ')' :
                (toStringTerm (NameEnv nameMap nextName m)))
            (M.toList funcs)
        )))
