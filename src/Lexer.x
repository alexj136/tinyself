{
module Lexer where

import Name
import qualified Data.Map as M
}

%wrapper "posn"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+                ;
    \#.*\n                 ; -- Single line comments with '#'
    "["                    { \p s -> Right ( TK_LSquare , p ) }
    "]"                    { \p s -> Right ( TK_RSquare , p ) }
    ","                    { \p s -> Right ( TK_Comma   , p ) }
    "<-"                   { \p s -> Right ( TK_LArrow  , p ) }
    ":"                    { \p s -> Right ( TK_Colon   , p ) }
    "("                    { \p s -> Right ( TK_LParen  , p ) }
    ")"                    { \p s -> Right ( TK_RParen  , p ) }
    "$"                    { \p s -> Right ( TK_Dollar  , p ) }
    "."                    { \p s -> Right ( TK_Dot     , p ) }
    $alpha [$alnum \_]*    { \p s -> Left  ( s          , p ) }

{
data TokenKind
    = TK_LSquare
    | TK_RSquare
    | TK_Comma
    | TK_LArrow
    | TK_Colon
    | TK_LParen
    | TK_RParen
    | TK_Dollar
    | TK_Dot
    | TK_Name Name
    deriving (Show, Eq)

type Token = (TokenKind, AlexPosn)
type AlmostToken = Either (String, AlexPosn) Token

makeTokens :: [AlmostToken] -> (M.Map String Name, Name, [Token])
makeTokens []         = (M.empty, Name 0, [])
makeTokens (atk:atks) =
    let (nameMap, nextName, atks') = makeTokens atks in case atk of
        Right tk       -> (nameMap, nextName, (tk:atks'))
        Left  (s, pos) -> case M.lookup s nameMap of
            Just nm -> (nameMap, nextName, ((TK_Name nm, pos):atks'))
            Nothing -> (M.insert s nextName nameMap,
                next nextName, (TK_Name nextName, pos):atks')

scan :: String -> NameEnv [Token]
scan s = let (nameMap, nextName, tokens) = makeTokens (alexScanTokens s) in
    NameEnv ((M.fromList . map (\(k,v) -> (v,k)) .  M.toList) nameMap)
        nextName tokens
}
