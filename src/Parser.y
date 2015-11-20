{
module Parser where

import Name
import Lexer
import Syntax
import qualified Data.Map as M
}

%name parse OBJ

%tokentype { Token      }
%error     { parseError }

%token
    lsquare { ( TK_LSquare   , _ ) }
    rsquare { ( TK_RSquare   , _ ) }
    comma   { ( TK_Comma     , _ ) }
    larrow  { ( TK_LArrow    , _ ) }
    colon   { ( TK_Colon     , _ ) }
    lparen  { ( TK_LParen    , _ ) }
    rparen  { ( TK_RParen    , _ ) }
    dollar  { ( TK_Dollar    , _ ) }
    dot     { ( TK_Dot       , _ ) }
    int     { ( (TK_Int  $$) , _ ) }
    name    { ( (TK_Name $$) , _ ) }
%%

OBJ :: { Term }
OBJ
    : lsquare rsquare               { Lit M.empty             }
    | lsquare name colon FN LITCONT { Lit (M.insert $2 $4 $5) }
    | name                          { Var $1                  }
    | OBJ dot name                  { Call $1 $3              }
    | OBJ larrow name FN            { Assign $1 $3 $4         }
    | int                           { IntObj $1 M.empty       }

FN :: { Function }
FN : dollar lparen name rparen OBJ { ($3, $5) }

LITCONT :: { M.Map Name Function }
LITCONT
    : rsquare                     { M.empty           }
    | comma name colon FN LITCONT { M.insert $2 $4 $5 }

{
parseError :: [Token] -> a
parseError []                       = error "Reached end of file while parsing"
parseError ((_, AlexPn _ y x):rest) =
    error $ concat ["Parse error on line ", show y, ", column ", show x,"."]
}
