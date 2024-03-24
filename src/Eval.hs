module Eval (runPlayer) where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Scope as Scope
import DgState(DgState)
import qualified DgState
import qualified Trigger
import qualified Entity
import qualified Room
import qualified Door
import qualified Action
import qualified Command
import qualified Stmt
import qualified Declare
import qualified Assign
import qualified While
import qualified AssignStat
import qualified If
import qualified Func
import qualified Expr
import qualified Dice

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

intToBool :: Int -> Bool
intToBool x = x /= 0

isDead :: Entity.Entity -> DgState -> (Bool, DgState)
isDead e s = evalBoolExpr (Entity.alive e) (DgState.updateScope (Entity.statsToScope e) s)

checkPlayerDeath :: DgState -> DgState
checkPlayerDeath s =
    let
        (dead, newState) = isDead (DgState.player s) s
    in
        if
            dead
        then
            DgState.setRunning False newState
        else
            newState

checkEntityDeath :: DgState -> Entity.Entity -> DgState
checkEntityDeath s e =
    let
        (dead, newState) = isDead e s
    in
        if
            dead
        then
            DgState.removeEntity (Entity.name e) newState
        else
            newState

checkAllEntityDeath :: DgState -> DgState
checkAllEntityDeath s = foldl checkEntityDeath s (DgState.getEntities s)

checkDeath :: DgState -> DgState
checkDeath s = checkAllEntityDeath (checkPlayerDeath s)

runNpcs :: DgState -> DgState
runNpcs s =
    let
        nameList = DgState.getEntityNames s
    in
        foldl runNpcAndCheckDeath s nameList

runNpcAndCheckDeath :: DgState -> String -> DgState
runNpcAndCheckDeath s name = checkDeath (runNpc (DgState.lookupEntity name s) s)

runNpc :: Maybe Entity.Entity -> DgState -> DgState
runNpc Nothing s = DgState.updateMsg "NPC not found" s
runNpc (Just e) s =
    let
        (cmd, newState) = getNpcBehaviour e s
    in
        runCmd (Entity.name e) (Just cmd) newState

getNpcBehaviour :: Entity.Entity -> DgState -> (Command.Command, DgState)
getNpcBehaviour e s =
    getCommand
        (Entity.behaviour e)
        (Entity.defaultBehaviour e)
        (DgState.updateScope (Entity.statsToScope e) s)

getCommand :: [(Expr.Expr, Command.Command)] -> Command.Command -> DgState -> (Command.Command, DgState)
getCommand [] db s = (db, s)
getCommand ((expr, cmd):xs) db s =
    let
        (result, newState) = evalBoolExpr expr s
    in
        if
            result
        then
            (cmd, newState)
        else
            getCommand xs db newState

runPlayer :: String -> Maybe Command.Command -> DgState -> DgState
runPlayer src c s = checkDeath (runCmd src c s)

runCmd :: String -> Maybe Command.Command -> DgState -> DgState
rumCmd _ Nothing s = DgState.updateMsg "Invalid command" s
runCmd src (Just cmd) s =
    let
        target = if (Command.target cmd) == "self" then src else (Command.target cmd)
        newState = DgState.updateSourceAndTarget src target s
    in
        if src == "player" then
            case (Command.name cmd) of
                "take" -> runTakeCmd newState cmd
                "open" -> runOpenCmd newState cmd
                _ -> runActionCmd newState cmd
        else
            runActionCmd newState cmd

runOpenCmd :: DgState -> Command.Command -> DgState
runOpenCmd s c =
    case
        DgState.getDoor (Command.target c) s
    of
        Nothing -> DgState.updateMsg "Door not found" s
        (Just d) -> tryOpenDoor d s

tryOpenDoor :: Door.Door -> DgState -> DgState
tryOpenDoor d s =
    let
        (result, newState) =
            evalBoolExpr (Door.req d) (DgState.updateScope (Entity.itemAttribsToScope (DgState.player s)) s)
    in
        if
            result
        then
            DgState.updateCurrentRoom (Door.to d) newState
        else
            newState

runTakeCmd :: DgState -> Command.Command -> DgState
runTakeCmd s cmd =
    let
        (item, newRoom) = Room.takeItem (Command.target cmd) (DgState.getCurrentRoom s)
    in
        case item of
            Nothing -> DgState.updateMsg "Item not found" s
            (Just i) -> DgState.takeItem i newRoom s

runActionCmd :: DgState -> Command.Command -> DgState
runActionCmd s c =
    let
        source = DgState.lookupSource s
        target = DgState.lookupTarget s
        (action, args) = Entity.lookupAction (Command.name c) (Command.using c) (DgState.lookupSource s)
    in
        runTriggers (runAction s target action args) c target

runAction :: DgState -> Maybe Entity.Entity -> Maybe Action.Action -> Map String Expr.Expr -> DgState
runAction s Nothing _ _ = DgState.updateMsg "Target not found" s
runAction s _ Nothing _ = DgState.updateMsg "Action not found" s
runAction s (Just e) (Just a) args =
    Eval.evalStmtBlock
        (Action.stmts a)
        (DgState.updateScope (Scope.fromArgs args) s)

runTriggers :: DgState -> Command.Command -> Maybe Entity.Entity -> DgState
runTriggers s _ Nothing = DgState.updateMsg "Could not run triggers, target not found" s
runTriggers s c (Just e) =
    foldl
        (runTrigger (Command.name c))
        (DgState.swapSourceAndTarget s)
        (Entity.getTriggers e)

runTrigger :: String -> DgState -> Trigger.Trigger -> DgState
runTrigger a s t =
    let
        (cond, newState) =
            evalBoolExpr
                (Trigger.on t)
                (
                    DgState.updateScope
                        (
                            Scope.singletonWithDefault
                                a
                                (Expr.IntExpr 1)
                                (Expr.IntExpr 0)
                        )
                        s
                )
    in
        if
            cond
        then
            Eval.evalStmtBlock
                (Trigger.stmts t)
                newState
        else
            newState

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
    DgState.updateScope
        (Scope.add (DgState.scope state) (Declare.var d) (Declare.val d))
        state

evalAssign :: Assign.Assign -> DgState -> DgState
evalAssign a state =
    DgState.updateScope
        (Scope.update (DgState.scope state) (Assign.var a) (Assign.val a))
        state

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

evalBoolExpr :: Expr.Expr -> DgState -> (Bool, DgState)
evalBoolExpr e s =
    let
        (val, state) = evalExpr e s
    in
        (intToBool val, state)

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
evalExpr (Expr.VarExpr v) state =
    let expr = Scope.lookup (DgState.scope state) v in evalExpr expr state
evalExpr (Expr.StatExpr p) state = (DgState.getPropVal p state, state)
evalExpr (Expr.DiceExpr d) state =
    let
        (val, newGen) = Dice.roll (DgState.rng state) d
    in
        (val, DgState.updateGen newGen state)
