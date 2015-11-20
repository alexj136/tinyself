module Main where

import Name
import Lexer
import Parser
import Syntax
import Interpreter
import System.Environment (getArgs)

main = do
    args <- getArgs
    if length args == 1 then
        do {
            fileText <- readFile (args !! 0);
            let tokens = scan fileText
                ast    = fmap parse tokens
            in putStrLn $ case fmap interpret ast of
                { NameEnv _ _ (Just m) -> show $ Lit m
                ; NameEnv _ _ Nothing  -> "Runtime error."
                }
        }
    else
        putStrLn "Bad command syntax."
