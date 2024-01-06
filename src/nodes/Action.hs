module Action where

import Expr(Expr)
import Stmt(Stmt)

data Action = Action {
    name :: String,
    targets :: Expr,
    stmts :: [Stmt]
} deriving Show

