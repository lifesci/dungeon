module Item (
    Item(..),
    fromTemplate,
    toString
) where

import qualified Expr
import qualified Action
import qualified ItemTemplate
import qualified RoomTemplateItem
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Lib(join, applyTabs)

data Item = Item {
    name :: String,
    attribs :: Set String,
    args :: Map String Expr.Expr,
    actions :: Map String Action.Action
} deriving Show

toString :: Int -> Item -> String
toString t i = join
    "\n"
    (
        (applyTabs [name i] t)
        ++ (applyTabs ["Attributes"] (t+1))
        ++ (applyTabs (Set.elems (attribs i)) (t+2))
        ++ (applyTabs ["Actions"] (t+1))
        ++ (applyTabs (Map.keys (actions i)) (t+2))
    )

fromTemplate :: Maybe ItemTemplate.ItemTemplate -> RoomTemplateItem.RoomTemplateItem -> Item
fromTemplate Nothing _ = error "Item template not found"
fromTemplate (Just t) rti = Item {
    name=RoomTemplateItem.name rti,
    attribs=Set.fromList (ItemTemplate.attribs t),
    args=Map.fromList (zip (ItemTemplate.args t) (RoomTemplateItem.args rti)),
    actions=ItemTemplate.actions t
}

