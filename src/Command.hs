module Command (
    Command(..),
    parse
) where

import Lib(split)

data Command = Command {
    name :: String,
    target :: String,
    item :: Maybe String
} deriving Show

parse :: String -> Maybe Command
parse str = parse' (split ' ' str) where
    parse' (s:t:[]) = Just (Command { name=s, target=t, item=Nothing })
    parse' (s:t:"using":i:[]) = Just (Command { name=s, target=t, item=(Just i) })
    parse' _ = Nothing

