module Dungeon (
    Dungeon (..),
) where

import Data.Map (Map)
import EntityTemplate (EntityTemplate)
import ItemTemplate (ItemTemplate)
import RoomTemplate (RoomTemplate)

data Dungeon = Dungeon
    { name :: String
    , statblock :: Map String Int
    , playerTemplate :: EntityTemplate
    , enemyTemplates :: Map String EntityTemplate
    , itemTemplates :: Map String ItemTemplate
    , roomTemplates :: [RoomTemplate]
    }
    deriving (Show)
