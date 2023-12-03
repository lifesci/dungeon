module Eval where

import Parser
import Data.Map(Map)
import qualified Data.Map as Map

data GameState = GameState {
    currentRoom :: String,
    rooms :: Map String RoomNode,
    scope :: Scope
} deriving Show

data Scope = Scope (Map String Int) (Maybe Scope) deriving Show

emptyScope :: Scope
emptyScope = (Scope Map.empty Nothing)

addScopeVar :: Scope -> String -> Int -> Scope
addScopeVar (Scope vars parent) var val = Scope (Map.insert var val vars) parent

updateScopeVar :: Scope -> String -> Int -> Scope
updateScopeVar (Scope vars Nothing) var val = case (Map.lookup var vars) of
    Nothing -> error "Referenced unbound variable"
    (Just x) -> Scope (Map.insert var val vars) Nothing
updateScopeVar (Scope vars (Just parent)) var val = case (Map.lookup var vars) of
    Nothing -> Scope vars (Just (updateScopeVar parent var val))
    (Just x) -> Scope (Map.insert var val vars) (Just parent)

addScope :: Scope -> Scope
addScope x = (Scope Map.empty (Just x))

parentScope :: Scope -> Maybe Scope
parentScope (Scope _ y) = y

searchScope :: Scope -> String -> Int
searchScope (Scope vars Nothing) var = case (Map.lookup var vars) of
    Nothing -> error "Referenced unbound variable"
    (Just x) -> x
searchScope (Scope vars (Just parent)) var = case (Map.lookup var vars) of
    Nothing -> (searchScope parent var)
    (Just x) -> x

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

intToBool :: Int -> Bool
intToBool x = x /= 0

evalStmt :: StmtNode -> GameState -> GameState
evalStmt (DeclareNode d) game = GameState {
    currentRoom=(currentRoom game),
    rooms=(rooms game),
    scope=(evalDeclare (declareVar d) (declareVal d) (scope game))
}
evalStmt (AssignNode a) game =  GameState {
    currentRoom=(currentRoom game),
    rooms=(rooms game),
    scope=evalDeclare (assignVar a) (assignVal a) (scope game)
}
evalStmt x y = error "Not implemented"

evalDeclare :: String -> ExprNode -> Scope -> Scope
evalDeclare var val scope = addScopeVar scope var (evalExpr val scope)

and' :: Int -> Int -> Int
and' x y = boolToInt ((intToBool x) && intToBool y)

or' :: Int -> Int -> Int
or' x y = boolToInt ((intToBool x) || intToBool y)

gt :: Int -> Int -> Int
gt x y = boolToInt (x > y)

lt :: Int -> Int -> Int
lt x y = boolToInt (x < y)

gte :: Int -> Int -> Int
gte x y = boolToInt (x >= y)

lte :: Int -> Int -> Int
lte x y = boolToInt (x <= y)

evalExpr :: ExprNode -> Scope -> Int
evalExpr (BinOpNode op x y) scope = let
    f = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
        Mod -> mod
        Or -> or'
        And -> and'
        Gt -> gt
        Lt -> lt
        Gte -> gte
        Lte -> lte
    in
        f (evalExpr x scope) (evalExpr y scope)

evalExpr (UnOpNode op x) scope = case op of
    Neg -> -(evalExpr x scope)
    Not -> boolToInt (not (intToBool (evalExpr x scope)))
evalExpr (IntNode x) scope = x
evalExpr (IdNode id) scope = searchScope scope id
evalExpr (PropNode p) scope = 1
evalExpr (DiceNode d) scope = 1
