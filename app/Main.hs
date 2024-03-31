module Main (main) where

import Parser(parse)
import Lexer(alexScanTokens)
import DgState(DgState, msg, running)
import qualified DgState
import qualified Command
import qualified Eval
import System.Random(newStdGen)

main :: IO ()
main = do
    gen <- newStdGen
    print "Enter file name"
    fileName <- getLine
    s <- readFile fileName
    run (DgState.buildState gen (parse (alexScanTokens s)))

run :: DgState -> IO ()
run state = do
    putStr (DgState.toString state 0)
    cmd <- getLine
    let playerResult = Eval.runPlayer (Command.parse cmd) state
    putStr ((msg playerResult) ++ "\n")
    let npcResult = Eval.runNpcs playerResult
    putStr ((msg npcResult) ++ "\n")
    if running npcResult then run npcResult else putStr "Game Over\n"

