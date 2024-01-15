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
import Lib(join)

data Item = Item {
    name :: String,
    attribs :: Set String,
    args :: Map String Expr.Expr,
    actions :: Map String Action.Action
} deriving Show

toString :: Item -> String
toString i = join
    "\n"
    [
        (name i),
        "Attributes: " ++ (join ", " (Set.elems (attribs i))),
        "Actions: " ++ (join ", " (Map.keys (actions i)))
    ]

fromTemplate :: Maybe ItemTemplate.ItemTemplate -> RoomTemplateItem.RoomTemplateItem -> Item
fromTemplate Nothing _ = error "Item template not found"
fromTemplate (Just t) rti = Item {
    name=RoomTemplateItem.name rti,
    attribs=Set.fromList (ItemTemplate.attribs t),
    args=Map.fromList (zip (ItemTemplate.args t) (RoomTemplateItem.args rti)),
    actions=ItemTemplate.actions t
}

