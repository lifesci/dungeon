module Stat (Stat (..)) where

data Stat = Stat
    { owner :: String
    , name :: String
    }
    deriving (Show)
