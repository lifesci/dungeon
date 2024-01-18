module Room(Room(..), fromTemplate, toString) where

import qualified Entity
import qualified Item
import qualified EntityTemplate
import qualified ItemTemplate
import qualified RoomTemplate
import qualified RoomTemplateEntity
import qualified RoomTemplateItem
import Door(Door)
import Lib(listToMap, join)
import Data.Map(Map)
import qualified Data.Map as Map

data Room = Room {
    name :: String,
    entities :: Map String Entity.Entity,
    items :: Map String Item.Item,
    doors :: [Door]
} deriving Show

toString :: Room -> String
toString r = join
    "\n"
    (
        [
            "Room: " ++ (name r),
            "Entities"
        ]
        ++ (map (Entity.toString 0) (Map.elems (entities r)))
        ++ ["Items"]
        ++ (map (Item.toString 0) (Map.elems (items r)))
    )

fromTemplate :: Map String EntityTemplate.EntityTemplate -> Map String ItemTemplate.ItemTemplate -> Map String Int -> RoomTemplate.RoomTemplate -> Room
fromTemplate etm itm sb rt = Room {
    name=RoomTemplate.name rt,
    entities=listToMap
        (
            map
                (
                    \rte -> Entity.fromTemplate
                        (Map.lookup (RoomTemplateEntity.template rte) etm)
                        sb
                        rte
                )
                (RoomTemplate.entities rt)
        )
        Entity.name
        id,
    items=listToMap
        (
            map
                (
                    \rti -> Item.fromTemplate
                        (Map.lookup (RoomTemplateItem.template rti) itm)
                        rti
                )
                (RoomTemplate.items rt)
        )
        Item.name
        id,
    doors=RoomTemplate.doors rt
}

