module RoomTemplateItem (RoomTemplateItem (..)) where

import Expr (Expr)

data RoomTemplateItem = RoomTemplateItem
    { name :: String
    , template :: String
    , attribs :: [String]
    , args :: [Expr]
    }
    deriving (Show)
