module Assign where

import Expr(Expr)

data Assign = Assign {
    var :: String,
    val :: Expr
} deriving Show

