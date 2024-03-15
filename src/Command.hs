module Command (
    Command(..),
    parse,
    empty
) where

import Lib(split)

data Command = Command {
    name :: String,
    target :: String,
    using :: Maybe String
} deriving Show

parse :: String -> Maybe Command
parse str = parse' (split ' ' str) where
    parse' (s:t:[]) = Just (Command { name=s, target=t, using=Nothing })
    parse' (s:t:"using":i:[]) = Just (Command { name=s, target=t, using=(Just i) })
    parse' _ = Nothing

empty :: Command
empty = Command { name="", target="", using=Nothing }

