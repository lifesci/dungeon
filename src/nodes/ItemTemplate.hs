module ItemTemplate(ItemTemplate(..)) where

import Data.Map(Map)
import Action(Action)
import Trigger(Trigger)

data ItemTemplate = ItemTemplate {
    name :: String,
    attribs :: [String],
    args :: [String],
    actions :: Map String Action,
    triggers :: Map String Trigger
} deriving Show

