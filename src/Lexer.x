{
module Lexer where
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
    "["                    { \p s -> ( TK_LSquare      , pos p ) }
    "]"                    { \p s -> ( TK_RSquare      , pos p ) }
    ","                    { \p s -> ( TK_Comma        , pos p ) }
    "<-"                   { \p s -> ( TK_LArrow       , pos p ) }
    ":"                    { \p s -> ( TK_Colon        , pos p ) }
    "("                    { \p s -> ( TK_LParen       , pos p ) }
    ")"                    { \p s -> ( TK_RParen       , pos p ) }
    "$"                    { \p s -> ( TK_Dollar       , pos p ) }
    "."                    { \p s -> ( TK_Dot          , pos p ) }
    $alpha [$alnum \_]*    { \p s -> ( TK_Name (read s), pos p ) }

{
type Token = (TokenKind, TokenPos)
type TokenPos = (Int, Int)

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
    | TK_Name Int
    deriving (Show, Eq, Ord)

getX :: Token -> Int
getX (_, (_, x)) = x
getY :: Token -> Int
getY (_, (y, _)) = y

-- The lexer function
scan :: String -> [Token]
scan = alexScanTokens

-- Extract token coordinates from AlexPosn object
pos :: AlexPosn -> (Int, Int)
pos (AlexPn i j k) = (j, k)
}
