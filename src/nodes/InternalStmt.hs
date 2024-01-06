module InternalStmt(Stmt(..), If(..), While(..)) where

import Declare(Declare)
import Assign(Assign)
import AssignStat(AssignStat)
import Func(Func)
import Expr(Expr)

data If = If {
    conds :: [(Expr, [Stmt])]
} deriving Show

data While = While {
    cond :: Expr,
    stmts :: [Stmt]
} deriving Show

data Stmt
    = DeclareStmt Declare
    | AssignStmt Assign
    | AssignStatStmt AssignStat
    | FuncStmt Func
    | WhileStmt While
    | IfStmt If deriving Show

