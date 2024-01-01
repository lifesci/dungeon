module Eval (buildState, GameState) where

import Parser
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
buildState gen dgn = GameState {
    scope=Scope.empty,
    currentRoom="test",
    game=dgn,
    running=True,
    rng=gen
}

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

intToBool :: Int -> Bool
intToBool x = x /= 0



evalStmtBlock :: [StmtNode] -> GameState -> GameState
evalStmtBlock [] state = GameState {
    currentRoom=(currentRoom state),
    scope=(Scope.parent (scope state)),
    game=(game state),
    running=(running state),
    rng=(rng state)
}
evalStmtBlock (x:xs) state =
    if
        (running state)
    then
        evalStmtBlock xs (evalStmt state x)
    else
        state

evalStmt :: GameState -> StmtNode -> GameState
evalStmt state (DeclareNode d) =
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
evalStmt state (AssignNode a) =
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
evalStmt state (WhileNode w) =
    let
        (condVal, newRng) = evalExpr (whileCond w) (scope state) (rng state)
    in
        if
            intToBool condVal
        then
            evalStmt
                (
                    evalStmtBlock
                        (whileStmts w)
                        GameState {
                            currentRoom=(currentRoom state),
                            scope=(Scope.push (scope state)),
                            game=(game state),
                            running=(running state),
                            rng=newRng
                        }
                )
                (WhileNode w)
        else
            GameState {
                currentRoom=(currentRoom state),
                scope=(scope state),
                game=(game state),
                running=(running state),
                rng=newRng
            }
evalStmt state (IfNode i) = evalIf (ifConds i) state
evalStmt _ _ = error "Not implemented"

evalIf :: [(ExprNode, [StmtNode])] -> GameState -> GameState
evalIf [] state = state
evalIf ((expr, stmts):xs) state =
    let
        (condVal, newGen) = evalExpr expr (scope state) (rng state)
    in
        if
            intToBool condVal
        then
            evalStmtBlock stmts GameState {
                currentRoom=(currentRoom state),
                scope=(Scope.push (scope state)),
                game=(game state),
                running=(running state),
                rng=newGen
            }
        else
            evalIf xs GameState {
                currentRoom=(currentRoom state),
                scope=(scope state),
                game=(game state),
                running=(running state),
                rng=newGen
            }

evalDeclare :: Declare -> Scope -> StdGen -> (Scope, StdGen)
evalDeclare d scp gen =
    let
        (val, newRng) = evalExpr (declareVal d) scp gen
    in
        (Scope.add scp (declareVar d) val, newRng)

evalAssign :: Assign -> Scope -> StdGen -> (Scope, StdGen)
evalAssign a scp gen =
    let
        (val, newRng) = evalExpr (assignVal a) scp gen
    in
        (Scope.update scp (assignVar a) val, newRng)

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
evalExpr (BinOpNode op x y) scp gen = let
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
    (lVal, lRng) = evalExpr x scp gen
    in
        let
            (rVal, rRng) = evalExpr y scp lRng
        in
            ((f lVal rVal), rRng)

evalExpr (UnOpNode op x) scp gen =
    let
        (val, newRng) = evalExpr x scp gen
    in
        case op of
            Neg -> (-(val), newRng)
            Not -> (boolToInt (not (intToBool (val))), newRng)
evalExpr (IntNode x) scp gen = (x, gen)
evalExpr (IdNode idn) scp gen = (Scope.search scp idn, gen)
evalExpr (PropNode p) scp gen = (1, gen)
evalExpr (DiceNode d) scp gen = randomR ((diceCount d), (diceCount d)*(diceSize d)) gen
