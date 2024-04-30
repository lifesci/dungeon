module RoomTemplate (RoomTemplate (..)) where

import Door (Door)
import RoomTemplateEntity (RoomTemplateEntity)
import RoomTemplateItem (RoomTemplateItem)

data RoomTemplate = RoomTemplate
    { name :: String
    , entities :: [RoomTemplateEntity]
    , items :: [RoomTemplateItem]
    , doors :: [Door]
    }
    deriving (Show)
