module Eval where

import Parser
import Scope(Scope)
import qualified Scope as Scope
import DgState(DgState)
import qualified DgState as DgState
import System.Random(StdGen, randomR)

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

intToBool :: Int -> Bool
intToBool x = x /= 0

evalStmtBlock :: [StmtNode] -> DgState -> DgState
evalStmtBlock [] state = DgState.leaveScope state
evalStmtBlock (x:xs) state =
    if
        (DgState.running state)
    then
        evalStmtBlock xs (evalStmt state x)
    else
        state

evalStmt :: DgState -> StmtNode -> DgState
evalStmt state (DeclareNode d) = evalDeclare d state
evalStmt state (AssignNode a) = evalAssign a state
evalStmt state (WhileNode w) =
    let
        (condVal, newState) = evalExpr (whileCond w) state
    in
        if
            intToBool condVal
        then
            evalStmt
                (
                    evalStmtBlock
                        (whileStmts w)
                        (DgState.enterScope newState)
                )
                (WhileNode w)
        else
            newState
evalStmt state (IfNode i) = evalIf (ifConds i) state
evalStmt state (FuncNode f) = evalFunc f state
evalStmt _ _ = error "Not implemented"

evalFunc :: Func -> DgState -> DgState
evalFunc Func{funcName="quit"} state = DgState.setRunning False state
evalFunc _ _ = error "Unknown function"

evalIf :: [(ExprNode, [StmtNode])] -> DgState -> DgState
evalIf [] state = state
evalIf ((expr, stmts):xs) state =
    let
        (condVal, newState) = evalExpr expr state
    in
        if
            intToBool condVal
        then
            evalStmtBlock stmts (DgState.enterScope newState)
        else
            evalIf xs newState

evalDeclare :: Declare -> DgState -> DgState
evalDeclare d state =
    let
        (val, newState) = evalExpr (declareVal d) state
    in
        DgState.updateScope (Scope.add (DgState.scope newState) (declareVar d) val) newState

evalAssign :: Assign -> DgState -> DgState
evalAssign a state =
    let
        (val, newState) = evalExpr (assignVal a) state
    in
        DgState.updateScope (Scope.update (DgState.scope newState) (assignVar a) val) newState

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

evalExpr :: ExprNode -> DgState -> (Int, DgState)
evalExpr (BinOpNode op x y) state = let
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
    (lVal, lState) = evalExpr x state
    in
        let
            (rVal, rState) = evalExpr y lState
        in
            ((f lVal rVal), rState)

evalExpr (UnOpNode op x) state =
    let
        (val, newState) = evalExpr x state
    in
        case op of
            Neg -> (-(val), newState)
            Not -> (boolToInt (not (intToBool (val))), newState)
evalExpr (IntNode x) state = (x, state)
evalExpr (IdNode idn) state = (Scope.search (DgState.scope state) idn, state)
evalExpr (PropNode p) state = (1, state)
evalExpr (DiceNode d) state =
    let
        (val, newGen) = randomR ((diceCount d), (diceCount d)*(diceSize d)) (DgState.rng state)
    in
        (val, DgState.updateGen newGen state)
