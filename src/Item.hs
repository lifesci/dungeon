module Item (
    Item(..)
) where

import qualified Expr
import qualified Action
import Data.Set(Set)
import Data.Map(Map)

data Item = Item {
    name :: String,
    attribs :: Set String,
    args :: Map String Expr.Expr,
    actions :: Map String Action.Action
} deriving Show

