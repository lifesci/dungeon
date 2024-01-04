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
evalStmt state (DeclareNode d) =
    let
        (newScope, newGen) = evalDeclare d (DgState.scope state) (DgState.rng state)
    in
        DgState.updateScopeAndGen newScope newGen state
evalStmt state (AssignNode a) =
    let
        (newScope, newGen) = evalAssign a (DgState.scope state) (DgState.rng state)
    in
        DgState.updateScopeAndGen newScope newGen state
evalStmt state (WhileNode w) =
    let
        (condVal, newGen) = evalExpr (whileCond w) (DgState.scope state) (DgState.rng state)
    in
        if
            intToBool condVal
        then
            evalStmt
                (
                    evalStmtBlock
                        (whileStmts w)
                        (DgState.enterScope newGen state)
                )
                (WhileNode w)
        else
            DgState.updateGen newGen state
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
        (condVal, newGen) = evalExpr expr (DgState.scope state) (DgState.rng state)
    in
        if
            intToBool condVal
        then
            evalStmtBlock stmts (DgState.enterScope newGen state)
        else
            evalIf xs (DgState.updateGen newGen state)

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
