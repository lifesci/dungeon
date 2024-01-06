module AssignStat where

import Stat(Stat)
import Expr(Expr)

data AssignStat = AssignStat {
    stat :: Stat,
    val :: Expr
} deriving Show

