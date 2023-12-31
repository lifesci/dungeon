module Main (main) where

import Parser(parse)
import Lexer(alexScanTokens)
import Eval(buildState, GameState)
import System.Random(newStdGen)

main :: IO ()
main = do
    gen <- newStdGen
    print "Enter file name"
    fileName <- getLine
    s <- readFile fileName
    run (buildState gen (parse (alexScanTokens s)))

run :: GameState -> IO ()
run state = do
    print state
