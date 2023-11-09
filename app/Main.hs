module Main (main) where

import Lib
import Parser(parse)
import Lexer(alexScanTokens)

main :: IO ()
main = do
    print "Enter file name"
    fileName <- getLine
    s <- readFile fileName
    print (parse (alexScanTokens s))
