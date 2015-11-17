{
module Parser where

import Lexer
import Syntax
import qualified Data.Map as M
}

%name parse OBJ

%tokentype { Token      }
%error     { parseError }

%token
    lsquare { Token TK_LSquare   _ }
    rsquare { Token TK_RSquare   _ }
    comma   { Token TK_Comma     _ }
    larrow  { Token TK_LArrow    _ }
    colon   { Token TK_Colon     _ }
    lparen  { Token TK_LParen    _ }
    rparen  { Token TK_RParen    _ }
    dollar  { Token TK_Dollar    _ }
    dot     { Token TK_Dot       _ }
    name    { Token (TK_Name $$) _ }
%%

OBJ :: { Term }
OBJ
    : lsquare rsquare               { Lit M.empty                    }
    | lsquare name colon FN LITCONT { Lit (M.insert (Name $2) $4 $5) }
    | name                          { Var (Name $1)                  }
    | OBJ dot name                  { Call $1 (Name $3)              }
    | OBJ larrow name FN            { Assign $1 (Name $3) $4         }

FN :: { Function }
FN : dollar lparen name rparen OBJ { (Name $3, $5) }

LITCONT :: { M.Map Name Function }
LITCONT
    : rsquare                     { M.empty                  }
    | comma name colon FN LITCONT { M.insert (Name $2) $4 $5 }

{
parseError :: [Token] -> a
parseError []                = error "Reached end of file while parsing"
parseError (Token _ (AlexPn _ y x) : ts) =
    error $ concat ["Parse error on line ", show y, ", column ", show x,"."]
}
