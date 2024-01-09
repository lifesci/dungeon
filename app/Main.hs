module Main (main) where

import Parser(parse)
import Lexer(alexScanTokens)
import DgState(DgState)
import qualified DgState
import System.Random(newStdGen)

main :: IO ()
main = do
    gen <- newStdGen
    print "Enter file name"
    fileName <- getLine
    s <- readFile fileName
    -- print (parse (alexScanTokens s))
    run (DgState.buildState gen (parse (alexScanTokens s)))

run :: DgState -> IO ()
run state = do
    putStr (DgState.toString state)

