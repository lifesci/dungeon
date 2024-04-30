module ItemTemplate (ItemTemplate (..)) where

import Action (Action)
import Data.Map (Map)
import Trigger (Trigger)

data ItemTemplate = ItemTemplate
    { name :: String
    , attribs :: [String]
    , args :: [String]
    , actions :: Map String Action
    , triggers :: Map String Trigger
    }
    deriving (Show)
