module Syntax where

import qualified Data.List as L
import qualified Data.Map  as M

type Name = String
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
            map (\(l, (s, m)) -> l ++ ":$(" ++ s ++ ')' : show m)
                (M.toList mets)
            ))) ++ " ]"
        Var name            -> name
        Call obj name       -> show obj ++ '.' : name
        Assign obj name (s, bdy) -> show obj ++ " <- " ++ name ++
            ":$(" ++ s ++ ")" ++ show bdy
