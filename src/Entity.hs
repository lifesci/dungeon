module Entity (
    Entity(..),
    fromTemplate,
    playerFromTemplate,
    toString,
    takeItem,
    lookupAction
) where

import qualified EntityTemplate
import qualified RoomTemplateEntity
import qualified Expr
import qualified Action
import qualified Trigger
import qualified Item
import Lib(join, applyTabs)
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

lookupAction :: String -> Maybe String -> Entity -> Maybe Action.Action
lookupAction name Nothing e = Map.lookup name (actions e)
lookupAction name (Just item) e =
    case
        Map.lookup item (items e)
    of
        Nothing -> Nothing
        (Just i) -> Map.lookup name (Item.actions i)

statsToString :: Map String Int -> [String]
statsToString m = map statToString (Map.assocs m) where
    statToString (x, y) = x ++ ": " ++ (show y)

itemsToString :: Int -> Map String Item.Item -> [String]
itemsToString t m = map (Item.toString t) (Map.elems m)

toString :: Int -> Entity -> String
toString t e = join
    "\n"
    (
        (applyTabs [(show (eType e))] t)
        ++ (applyTabs [
            "Name: " ++ (name e),
            "Stats"
        ] (t+1))
        ++ (applyTabs (statsToString (stats e)) (t+2))
        ++ (applyTabs ["Actions"] (t+1))
        ++ (applyTabs (Map.keys (actions e)) (t+2))
        ++ (applyTabs ["Items"] (t+1))
        ++ (itemsToString (t+2) (items e))
    )

takeItem :: Item.Item -> Entity -> Entity
takeItem i e = Entity {
    eType=(eType e),
    name=(name e),
    args=(args e),
    stats=(stats e),
    alive=(alive e),
    actions=(actions e),
    triggers=(triggers e),
    items=(Map.insert (Item.name i) i (items e))
}

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

