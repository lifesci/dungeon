module Eval where

import Parser
import Data.Map(Map)
import qualified Data.Map as Map

data Scope = Scope (Map String Int) (Maybe Scope)

emptyScope :: Scope
emptyScope = (Scope Map.empty Nothing)

addScopeVar :: Scope -> String -> Int -> Scope
addScopeVar (Scope vars parent) var val = Scope (Map.insert var val vars) parent

addScope :: Scope -> Scope
addScope x = (Scope Map.empty (Just x))

parentScope :: Scope -> Maybe Scope
parentScope (Scope x y) = y

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

evalExpr :: ExprNode -> Scope -> Int
evalExpr (BinOpNode op x y) scope = case op of
    Add -> evalExpr x scope + evalExpr y scope
    Sub -> evalExpr x scope - evalExpr y scope
    Mul -> evalExpr x scope * evalExpr y scope
    Div -> evalExpr x scope `div` evalExpr y scope
    Mod -> evalExpr x scope `mod` evalExpr y scope
    Or -> boolToInt ((intToBool (evalExpr x scope)) || (intToBool (evalExpr y scope)))
    And -> boolToInt ((intToBool (evalExpr x scope)) && (intToBool (evalExpr y scope)))
    Gt -> boolToInt ((evalExpr x scope) > (evalExpr y scope))
    Lt -> boolToInt ((evalExpr x scope) < (evalExpr y scope))
    Gte -> boolToInt ((evalExpr x scope) >= (evalExpr y scope))
    Lte -> boolToInt ((evalExpr x scope) <= (evalExpr y scope))
evalExpr (UnOpNode op x) scope = case op of
    Neg -> -(evalExpr x scope)
    Not -> boolToInt (not (intToBool (evalExpr x scope)))
evalExpr (IntNode x) scope = x
evalExpr (IdNode id) scope = searchScope scope id
evalExpr (PropNode p) scope = 1
evalExpr (DiceNode d) scope = 1
