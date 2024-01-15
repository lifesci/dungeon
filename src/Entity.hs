module Entity (
    Entity(..),
    fromTemplate,
    playerFromTemplate,
    toString
) where

import qualified EntityTemplate
import qualified RoomTemplateEntity
import qualified Expr
import qualified Action
import qualified Trigger
import qualified Item
import Lib(join)
import Data.Map(Map)
import qualified Data.Map as Map

data Entity = Entity {
    eType :: EntityTemplate.EntityType,
    name :: String,
    args :: Map String Expr.Expr,
    stats :: Map String Int,
    alive :: Expr.Expr,
    actions :: Map String Action.Action,
    triggers :: Map String Trigger.Trigger,
    items :: Map String Item.Item
} deriving Show

statsToString :: Map String Int -> [String]
statsToString m = map statToString (Map.assocs m) where
    statToString (x, y) = x ++ ": " ++ (show y)

itemsToString :: Map String Item.Item -> [String]
itemsToString m = map Item.toString (Map.elems m)

toString :: Entity -> String
toString e = join
    "\n"
    (
        [
            "Name: " ++ (name e)
        ]
        ++ (statsToString (stats e))
        ++ [
            ("Actions: " ++ (join ", " (Map.keys (actions e)))),
            "Items"
        ]
        ++ (itemsToString (items e))
    )

fromTemplate :: Maybe EntityTemplate.EntityTemplate -> Map String Int -> RoomTemplateEntity.RoomTemplateEntity -> Entity.Entity
fromTemplate Nothing _ _ = error "Entity template not found"
fromTemplate (Just t) statblock rte = Entity {
    eType=(EntityTemplate.eType t),
    name=(RoomTemplateEntity.name rte),
    args=(
        Map.fromList(
            zip
                (EntityTemplate.args t)
                (RoomTemplateEntity.args rte)
        )
    ),
    stats=(statsFromTemplate t statblock),
    alive=(EntityTemplate.alive t),
    actions=(EntityTemplate.actions t),
    triggers=(EntityTemplate.triggers t),
    items=Map.empty
}

playerFromTemplate :: EntityTemplate.EntityTemplate -> Map String Int -> Entity.Entity
playerFromTemplate t statblock = Entity {
    eType=(EntityTemplate.eType t),
    name="player",
    args=Map.empty,
    stats=(statsFromTemplate t statblock),
    alive=(EntityTemplate.alive t),
    actions=(EntityTemplate.actions t),
    triggers=(EntityTemplate.triggers t),
    items=Map.empty
}

statsFromTemplate :: EntityTemplate.EntityTemplate -> Map String Int -> Map String Int
statsFromTemplate t statblock =
    Map.union (Map.intersection (EntityTemplate.stats t) statblock) statblock

