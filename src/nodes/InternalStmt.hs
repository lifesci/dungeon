module InternalStmt (Stmt (..), If (..), While (..)) where

import Assign (Assign)
import AssignStat (AssignStat)
import Declare (Declare)
import Expr (Expr)
import Func (Func)

data If = If
    { conds :: [(Expr, [Stmt])]
    }
    deriving (Show)

data While = While
    { cond :: Expr
    , stmts :: [Stmt]
    }
    deriving (Show)

data Stmt
    = DeclareStmt Declare
    | AssignStmt Assign
    | AssignStatStmt AssignStat
    | FuncStmt Func
    | WhileStmt While
    | IfStmt If
    deriving (Show)
