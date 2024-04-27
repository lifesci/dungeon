module Room(
    Room(..),
    fromTemplate,
    toString,
    takeItem,
    lookupEntity,
    getDoor,
    getEntityNames,
    getEntities,
    removeEntity
) where

import qualified Entity
import qualified Item
import qualified EntityTemplate
import qualified ItemTemplate
import qualified RoomTemplate
import qualified RoomTemplateEntity
import qualified RoomTemplateItem
import Door(Door)
import qualified Door
import Lib(listToMap, join, applyTabs, popMap)
import Data.Map(Map)
import qualified Data.Map as Map

data Room = Room {
    name :: String,
    entities :: Map String Entity.Entity,
    items :: Map String Item.Item,
    doors :: Map String Door
} deriving Show

lookupEntity :: String -> Room -> Maybe Entity.Entity
lookupEntity name r = Map.lookup name (entities r)

toString :: Int -> Room -> String
toString t r = join
    "\n"
    (
        (applyTabs ["Room"] t)
        ++ (applyTabs ["Name: " ++ (name r)] (t+1))
        ++ (applyTabs ["Entities"] (t+1))
        ++ (map (Entity.toString (t+2)) (Map.elems (entities r)))
        ++ (applyTabs ["Items"] (t+1))
        ++ (map (Item.toString (t+2)) (Map.elems (items r)))
        ++ (map (Door.toString (t+1)) (Map.elems (doors r)))
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
                        itm
                        rte
                )
                (RoomTemplate.entities rt)
        )
        Entity.name
        id,
    items=Item.templateListToInventory (RoomTemplate.items rt) itm,
    doors=(listToMap (RoomTemplate.doors rt) Door.name id)
}

updateItems :: Map String Item.Item -> Room -> Room
updateItems i r = r { items=i }

takeItem :: String -> Room -> (Maybe Item.Item, Room)
takeItem s r =
    let
        (item, rest) = Lib.popMap s (items r)
    in
        (item, updateItems rest r)

getDoor :: String -> Room -> Maybe Door
getDoor name room = Map.lookup name (doors room)

getEntityNames :: Room -> [String]
getEntityNames r = map (\(x, _) -> x) (Map.toList (entities r))

getEntities :: Room -> [Entity.Entity]
getEntities r = map (\(_, y) -> y) (Map.toList (entities r))

removeEntity :: String -> Room -> Room
removeEntity name r = r { entities=Map.delete name (entities r) }

