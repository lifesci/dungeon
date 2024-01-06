module Room(Room(..)) where

import Entity(Entity)
import Item(Item)
import Door(Door)
import Data.Map(Map)

data Room = Room {
    name :: String,
    entities :: Map String Entity,
    items :: Map String Item,
    doors :: [Door]
} deriving Show

