module Func where

import Expr(Expr)

data Func = Func {
    name :: String,
    args :: [Expr]
} deriving Show

