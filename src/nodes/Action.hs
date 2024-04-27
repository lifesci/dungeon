module Action(Action(..)) where

import Expr(Expr)
import Stmt(Stmt)

data Action = Action {
    name :: String,
    stmts :: [Stmt]
} deriving Show

