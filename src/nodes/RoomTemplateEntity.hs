module RoomTemplateEntity(RoomTemplateEntity(..)) where

import Expr(Expr)

data RoomTemplateEntity = RoomTemplateEntity {
    name :: String,
    template :: String,
    args :: [Expr]
} deriving Show

