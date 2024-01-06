module Entity (
    Entity(..)
) where

import qualified EntityTemplate
import qualified Expr
import qualified Action
import qualified Trigger
import qualified Item
import Data.Map(Map)

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

