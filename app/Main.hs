module Main (main) where

import Lib
import Parser(parse)
import Lexer(alexScanTokens)
import Eval(buildState, GameState)

main :: IO ()
main = do
    print "Enter file name"
    fileName <- getLine
    s <- readFile fileName
    run (buildState (parse (alexScanTokens s)))

run :: GameState -> IO ()
run state = do
    print state
