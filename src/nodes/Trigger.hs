module Trigger where

import Expr(Expr)
import Stmt(Stmt)

data Trigger = Trigger {
    name :: String,
    on :: Expr,
    stmts :: [Stmt]
} deriving Show

