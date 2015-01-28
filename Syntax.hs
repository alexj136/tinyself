module Syntax where

import qualified Data.List as L
import qualified Data.Map  as M

type Function = (Name, Term)

newtype Name = Name String deriving (Eq, Ord)
instance Show Name where
    show (Name n) = n

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