module Entity (
    Entity(..),
    fromTemplate,
    playerFromTemplate,
    toString,
    takeItem,
    lookupAction,
    itemAttribsToScope,
    getTriggers
) where

import qualified EntityTemplate
import qualified RoomTemplateEntity
import qualified Expr
import qualified Action
import qualified Trigger
import qualified Item
import Lib(join, applyTabs)
import Scope(Scope)
import qualified Scope as Scope
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

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

lookupAction :: String -> Maybe String -> Entity -> (Maybe Action.Action, Map String Expr.Expr)
lookupAction actionName Nothing e = (Map.lookup actionName (actions e), Map.empty)
lookupAction actionName (Just item) e =
    case
        Map.lookup item (items e)
    of
        Nothing -> (Nothing, Map.empty)
        (Just i) -> (Map.lookup actionName (Item.actions i), Item.args i)

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
        ++ (applyTabs ["Triggers"] (t+1))
        ++ (applyTabs (Map.keys (triggers e)) (t+2))
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

allItemAttribs :: Entity -> [String]
allItemAttribs e = Set.elems (foldl Set.union Set.empty (map Item.attribs (Map.elems (items e))))

itemAttribsToScope :: Entity -> Scope
itemAttribsToScope e = foldl Scope.addTrue Scope.emptyWithFalseDefault (allItemAttribs e)

getTriggers :: Entity -> [Trigger.Trigger]
getTriggers e =
    (Map.elems (triggers e))
    ++ (
        foldl
            (++)
            []
            (map (Map.elems . Item.triggers) (Map.elems (items e))
        )
    )

