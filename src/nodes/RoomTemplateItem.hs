module RoomTemplateItem(RoomTemplateItem(..)) where

import Expr(Expr)

data RoomTemplateItem = RoomTemplateItem {
    name :: String,
    template :: String,
    args :: [Expr]
} deriving Show

