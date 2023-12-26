module Eval where

import Parser
import Data.Map(Map)
import qualified Data.Map as Map
import Scope(Scope)
import qualified Scope as Scope

data GameState = GameState {
    currentRoom :: String,
    scope :: Scope,
    game :: DgNode,
    running :: Bool
} deriving Show

buildState :: DgNode -> GameState
buildState dgn = GameState {
    scope=Scope.empty,
    currentRoom="test",
    game=dgn,
    running=True
}

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

intToBool :: Int -> Bool
intToBool x = x /= 0

evalStmt :: StmtNode -> GameState -> GameState
evalStmt (DeclareNode d) state = GameState {
    currentRoom=(currentRoom state),
    scope=(evalDeclare (declareVar d) (declareVal d) (scope state)),
    game=(game state),
    running=(running state)
}
evalStmt (AssignNode a) state =  GameState {
    currentRoom=(currentRoom state),
    scope=evalDeclare (assignVar a) (assignVal a) (scope state),
    game=(game state),
    running=(running state)
}
evalStmt x y = error "Not implemented"

evalDeclare :: String -> ExprNode -> Scope -> Scope
evalDeclare var val scope = Scope.add scope var (evalExpr val scope)

and' :: Int -> Int -> Int
and' x y = boolToInt ((intToBool x) && intToBool y)

or' :: Int -> Int -> Int
or' x y = boolToInt ((intToBool x) || intToBool y)

gt :: Int -> Int -> Int
gt x y = boolToInt (x > y)

lt :: Int -> Int -> Int
lt x y = boolToInt (x < y)

gte :: Int -> Int -> Int
gte x y = boolToInt (x >= y)

lte :: Int -> Int -> Int
lte x y = boolToInt (x <= y)

evalExpr :: ExprNode -> Scope -> Int
evalExpr (BinOpNode op x y) scope = let
    f = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
        Mod -> mod
        Or -> or'
        And -> and'
        Gt -> gt
        Lt -> lt
        Gte -> gte
        Lte -> lte
    in
        f (evalExpr x scope) (evalExpr y scope)

evalExpr (UnOpNode op x) scope = case op of
    Neg -> -(evalExpr x scope)
    Not -> boolToInt (not (intToBool (evalExpr x scope)))
evalExpr (IntNode x) scope = x
evalExpr (IdNode id) scope = Scope.search scope id
evalExpr (PropNode p) scope = 1
evalExpr (DiceNode d) scope = 1
