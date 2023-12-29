module Eval where

import Parser
import Data.Map(Map)
import qualified Data.Map as Map
import Scope(Scope)
import qualified Scope as Scope
import System.Random(StdGen, randomR)

data GameState = GameState {
    currentRoom :: String,
    scope :: Scope,
    game :: DgNode,
    running :: Bool,
    rng :: StdGen
} deriving Show

buildState :: StdGen -> DgNode -> GameState
buildState rng dgn = GameState {
    scope=Scope.empty,
    currentRoom="test",
    game=dgn,
    running=True,
    rng=rng
}

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

intToBool :: Int -> Bool
intToBool x = x /= 0

evalStmt :: StmtNode -> GameState -> GameState
evalStmt (DeclareNode d) state =
    let
        (newScope, newRng) = evalDeclare d (scope state) (rng state)
    in
        GameState {
            currentRoom=(currentRoom state),
            scope=(newScope),
            game=(game state),
            running=(running state),
            rng=newRng
        }
evalStmt (AssignNode a) state =
    let
        (newScope, newRng) = evalAssign a (scope state) (rng state)
    in
        GameState {
            currentRoom=(currentRoom state),
            scope=newScope,
            game=(game state),
            running=(running state),
            rng=newRng
        }
evalStmt x y = error "Not implemented"

evalDeclare :: Declare -> Scope -> StdGen -> (Scope, StdGen)
evalDeclare d scope rng =
    let
        (val, newRng) = evalExpr (declareVal d) scope rng
    in
        (Scope.add scope (declareVar d) val, newRng)

evalAssign :: Assign -> Scope -> StdGen -> (Scope, StdGen)
evalAssign a scope rng =
    let
        (val, newRng) = evalExpr (assignVal a) scope rng
    in
        (Scope.update scope (assignVar a) val, newRng)

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

evalExpr :: ExprNode -> Scope -> StdGen -> (Int, StdGen)
evalExpr (BinOpNode op x y) scope rng = let
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
    (lVal, lRng) = evalExpr x scope rng
    in
        let
            (rVal, rRng) = evalExpr y scope lRng
        in
            ((f lVal rVal), rRng)

evalExpr (UnOpNode op x) scope rng =
    let
        (val, newRng) = evalExpr x scope rng
    in
        case op of
            Neg -> (-(val), newRng)
            Not -> (boolToInt (not (intToBool (val))), newRng)
evalExpr (IntNode x) scope rng = (x, rng)
evalExpr (IdNode id) scope rng = (Scope.search scope id, rng)
evalExpr (PropNode p) scope rng = (1, rng)
evalExpr (DiceNode d) scope rng = randomR ((diceCount d), (diceCount d)*(diceSize d)) rng
