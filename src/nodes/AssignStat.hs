module AssignStat (AssignStat (..)) where

import Expr (Expr)
import Stat (Stat)

data AssignStat = AssignStat
    { stat :: Stat
    , val :: Expr
    }
    deriving (Show)
