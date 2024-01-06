module RoomTemplate where

import Expr(Expr)
import RoomTemplateEntity(RoomTemplateEntity)
import RoomTemplateItem(RoomTemplateItem)
import Door(Door)

data RoomTemplate = RoomTemplate {
    name :: String,
    entities :: [RoomTemplateEntity],
    items :: [RoomTemplateItem],
    doors :: [Door]
} deriving Show

