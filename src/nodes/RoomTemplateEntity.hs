module RoomTemplateEntity (RoomTemplateEntity (..)) where

import Expr (Expr)
import RoomTemplateItem (RoomTemplateItem)

data RoomTemplateEntity = RoomTemplateEntity
    { name :: String
    , template :: String
    , args :: [Expr]
    , items :: [RoomTemplateItem]
    }
    deriving (Show)
