module EntityTemplate (EntityTemplate (..), EntityType (..)) where

import Action (Action)
import Command (Command)
import Data.Map (Map)
import Expr (Expr)
import RoomTemplateItem (RoomTemplateItem)
import Trigger (Trigger)

data EntityType = Player | Enemy deriving (Show)

data EntityTemplate = EntityTemplate
    { eType :: EntityType
    , name :: String
    , args :: [String]
    , stats :: Map String Int
    , alive :: Expr
    , actions :: Map String Action
    , triggers :: Map String Trigger
    , behaviour :: [(Expr, Command)]
    , defaultBehaviour :: Command
    , items :: [RoomTemplateItem]
    }
    deriving (Show)
