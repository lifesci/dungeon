module Action (Action (..)) where

import Stmt (Stmt)

data Action = Action
    { name :: String
    , stmts :: [Stmt]
    }
    deriving (Show)
