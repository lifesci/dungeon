module Scope (
    Scope,
    empty,
    add,
    addTrue,
    update,
    push,
    parent,
    lookup,
    singletonWithDefault,
    fromArgs,
    fromIntArgs,
    emptyWithFalseDefault,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Expr
import Prelude hiding (lookup)

data Cactus = Cactus (Map String Expr.Expr) (Maybe Cactus) deriving (Show)
data Scope = Scope Cactus (Maybe Expr.Expr) deriving (Show)

empty :: Scope
empty = Scope (Cactus Map.empty Nothing) Nothing

emptyWithDefault :: Expr.Expr -> Scope
emptyWithDefault d = Scope (Cactus Map.empty Nothing) (Just d)

emptyWithFalseDefault :: Scope
emptyWithFalseDefault = emptyWithDefault (Expr.IntExpr 0)

singletonWithDefault :: String -> Expr.Expr -> Expr.Expr -> Scope
singletonWithDefault var val d = Scope (Cactus (Map.singleton var val) Nothing) (Just d)

fromArgs :: Map String Expr.Expr -> Scope
fromArgs args = Scope (Cactus args Nothing) Nothing

fromIntArgs :: Map String Int -> Scope
fromIntArgs args = fromArgs (Map.map (\x -> Expr.IntExpr x) args)

add :: Scope -> String -> Expr.Expr -> Scope
add (Scope (Cactus vars p) d) var val =
    Scope
        (Cactus (Map.insert var val vars) p)
        d

addTrue :: Scope -> String -> Scope
addTrue (Scope (Cactus vars p) d) var =
    Scope
        (Cactus (Map.insert var (Expr.IntExpr 1) vars) p)
        d

update' :: Cactus -> String -> Expr.Expr -> Cactus
update' (Cactus vars Nothing) var val = case (Map.lookup var vars) of
    Nothing -> error "Referenced unbound variable"
    (Just _) -> Cactus (Map.insert var val vars) Nothing
update' (Cactus vars (Just p)) var val = case (Map.lookup var vars) of
    Nothing -> Cactus vars (Just (update' p var val))
    (Just _) -> Cactus (Map.insert var val vars) (Just p)

update :: Scope -> String -> Expr.Expr -> Scope
update (Scope c d) var val = Scope (update' c var val) d

push :: Scope -> Scope
push (Scope c d) = Scope (Cactus Map.empty (Just c)) d

parent :: Scope -> Scope
parent (Scope (Cactus x Nothing) d) = Scope (Cactus x Nothing) d
parent (Scope (Cactus _ (Just y)) d) = Scope y d

lookup' :: Cactus -> String -> Maybe Expr.Expr
lookup' (Cactus vars Nothing) var = Map.lookup var vars
lookup' (Cactus vars (Just p)) var = case (Map.lookup var vars) of
    Nothing -> (lookup' p var)
    (Just x) -> (Just x)

lookup :: Scope -> String -> Expr.Expr
lookup (Scope c Nothing) var = case (lookup' c var) of
    Nothing -> error "Referenced unbound variable"
    (Just x) -> x
lookup (Scope c (Just d)) var = case (lookup' c var) of
    Nothing -> d
    (Just x) -> x
