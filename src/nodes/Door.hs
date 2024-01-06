module Door where

import Expr(Expr)

data Door = Door {
    name :: String,
    to :: String,
    req :: Expr
} deriving Show

