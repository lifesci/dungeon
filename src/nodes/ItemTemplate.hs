module ItemTemplate(ItemTemplate(..)) where

import Data.Map(Map)
import Action(Action)

data ItemTemplate = ItemTemplate {
    name :: String,
    attribs :: [String],
    args :: [String],
    actions :: Map String Action
} deriving Show

