{
module Lexer where

import qualified Data.Map as M
}

%wrapper "monadUserState"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+                ;
    \#.*\n                 ; -- Single line comments with '#'
    "["                    { makeSimpleToken TK_LSquare }
    "]"                    { makeSimpleToken TK_RSquare }
    ","                    { makeSimpleToken TK_Comma   }
    "<-"                   { makeSimpleToken TK_LArrow  }
    ":"                    { makeSimpleToken TK_Colon   }
    "("                    { makeSimpleToken TK_LParen  }
    ")"                    { makeSimpleToken TK_RParen  }
    "$"                    { makeSimpleToken TK_Dollar  }
    "."                    { makeSimpleToken TK_Dot     }
    $alpha [$alnum \_]*    { makeNameToken              }

{

data AlexUserState = AlexUserState (M.Map String Int) Int deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState M.empty 0

alexEOF :: Alex Token
alexEOF = return $ Token TK_EOF $ error "position of EOF token"

getIntName :: AlexUserState -> String -> (AlexUserState, Int)
getIntName auState@(AlexUserState nameMap nextIntName) stringName =
    case M.lookup stringName nameMap of
        Just i  -> (auState, i)
        Nothing -> (AlexUserState (M.insert stringName nextIntName nameMap)
                (nextIntName + 1), nextIntName)

makeSimpleToken :: TokenKind -> AlexInput -> Int -> Alex Token
makeSimpleToken (TK_Name _) _            _ = error "makeSimpleToken of TK_Name"
makeSimpleToken tk          (p, _, _, _) _ = return $ Token tk p

makeNameToken :: AlexInput -> Int -> Alex Token
makeNameToken (p, _, _, input) len = Alex $ \aState -> let
    auState :: AlexUserState
    intName :: Int
    (auState, intName) = getIntName (alex_ust aState) (take len input)
    in Right (aState{alex_ust=auState}, Token (TK_Name intName) p)

data Token = Token TokenKind AlexPosn deriving Show

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
    | TK_EOF
    deriving (Show, Eq, Ord)
}
