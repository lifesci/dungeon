module EntityTemplate(EntityTemplate(..), EntityType(..)) where

import Data.Map(Map)
import Expr(Expr)
import Action(Action)
import Trigger(Trigger)
import Command(Command)

data EntityType = Player | Enemy deriving Show

data EntityTemplate = EntityTemplate {
    eType :: EntityType,
    name :: String,
    args :: [String],
    stats :: Map String Int,
    alive :: Expr,
    actions :: Map String Action,
    triggers :: Map String Trigger,
    behaviour :: [(Expr, Command)],
    defaultBehaviour :: Command
} deriving Show
