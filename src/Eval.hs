module Eval where

import Scope(Scope)
import qualified Scope as Scope
import DgState(DgState)
import qualified DgState as DgState
import qualified Stmt
import qualified Declare
import qualified Assign
import qualified While
import qualified AssignStat
import qualified If
import qualified Func
import qualified Expr
import qualified Dice
import System.Random(StdGen, randomR)

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

intToBool :: Int -> Bool
intToBool x = x /= 0

evalStmtBlock :: [Stmt.Stmt] -> DgState -> DgState
evalStmtBlock [] state = DgState.leaveScope state
evalStmtBlock (x:xs) state =
    if
        (DgState.running state)
    then
        evalStmtBlock xs (evalStmt state x)
    else
        state

evalStmt :: DgState -> Stmt.Stmt -> DgState
evalStmt state (Stmt.DeclareStmt d) = evalDeclare d state
evalStmt state (Stmt.AssignStmt a) = evalAssign a state
evalStmt state (Stmt.WhileStmt w) =
    let
        (condVal, newState) = evalExpr (While.cond w) state
    in
        if
            intToBool condVal
        then
            evalStmt
                (
                    evalStmtBlock
                        (While.stmts w)
                        (DgState.enterScope newState)
                )
                (Stmt.WhileStmt w)
        else
            newState
evalStmt state (Stmt.AssignStatStmt as) =
    let (val, newState) = evalExpr (AssignStat.val as) state in
        DgState.updateProp (AssignStat.stat as) val newState
evalStmt state (Stmt.IfStmt i) = evalIf (If.conds i) state
evalStmt state (Stmt.FuncStmt f) = evalFunc f state

evalFunc :: Func.Func -> DgState -> DgState
evalFunc Func.Func{Func.name="quit"} state = DgState.setRunning False state
evalFunc _ _ = error "Unknown function"

evalIf :: [(Expr.Expr, [Stmt.Stmt])] -> DgState -> DgState
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

evalDeclare :: Declare.Declare -> DgState -> DgState
evalDeclare d state =
    let
        (val, newState) = evalExpr (Declare.val d) state
    in
        DgState.updateScope (Scope.add (DgState.scope newState) (Declare.var d) val) newState

evalAssign :: Assign.Assign -> DgState -> DgState
evalAssign a state =
    let
        (val, newState) = evalExpr (Assign.val a) state
    in
        DgState.updateScope (Scope.update (DgState.scope newState) (Assign.var a) val) newState

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

evalExpr :: Expr.Expr -> DgState -> (Int, DgState)
evalExpr (Expr.BinOpExpr op x y) state = let
    f = case op of
        Expr.Add -> (+)
        Expr.Sub -> (-)
        Expr.Mul -> (*)
        Expr.Div -> div
        Expr.Mod -> mod
        Expr.Or -> or'
        Expr.And -> and'
        Expr.Gt -> gt
        Expr.Lt -> lt
        Expr.Gte -> gte
        Expr.Lte -> lte
    (lVal, lState) = evalExpr x state
    in
        let
            (rVal, rState) = evalExpr y lState
        in
            ((f lVal rVal), rState)

evalExpr (Expr.UnOpExpr op x) state =
    let
        (val, newState) = evalExpr x state
    in
        case op of
            Expr.Neg -> (-(val), newState)
            Expr.Not -> (boolToInt (not (intToBool (val))), newState)
evalExpr (Expr.IntExpr x) state = (x, state)
evalExpr (Expr.VarExpr vn) state = (Scope.search (DgState.scope state) vn, state)
evalExpr (Expr.StatExpr p) state = (DgState.getPropVal p state, state)
evalExpr (Expr.DiceExpr d) state = -- TODO: fix logic here
    let
        (val, newGen) = randomR ((Dice.count d), (Dice.count d)*(Dice.size d)) (DgState.rng state)
    in
        (val, DgState.updateGen newGen state)
