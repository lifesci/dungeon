module Declare(Declare(..)) where

import Expr(Expr)

data Declare = Declare {
    var :: String,
    val :: Expr
} deriving Show

