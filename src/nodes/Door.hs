module Door(Door(..), toString) where

import Expr(Expr)
import Lib(join, applyTab)

data Door = Door {
    name :: String,
    to :: String,
    req :: Expr
} deriving Show

toString :: Int -> Door -> String
toString tabs door = applyTab (join " " ["Door", (name door), "to", (to door)]) tabs

