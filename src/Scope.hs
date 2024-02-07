module Scope (
    Scope,
    empty,
    add,
    update,
    push,
    parent,
    lookup,
    singletonWithDefault
) where

import Prelude hiding (lookup)
import Data.Map(Map)
import qualified Data.Map as Map

data Cactus = Cactus (Map String Int) (Maybe Cactus) deriving Show
data Scope = Scope Cactus (Maybe Int) deriving Show

empty :: Scope
empty = Scope (Cactus Map.empty Nothing) Nothing

emptyWithDefault :: Int -> Scope
emptyWithDefault d = Scope (Cactus Map.empty Nothing) (Just d)

singleton :: String -> Int -> Scope
singleton var val = Scope (Cactus (Map.singleton var val) Nothing) Nothing

singletonWithDefault :: String -> Int -> Int -> Scope
singletonWithDefault var val d = Scope (Cactus (Map.singleton var val) Nothing) (Just d)

add :: Scope -> String -> Int -> Scope
add (Scope (Cactus vars p) d) var val =
    Scope
        (Cactus (Map.insert var val vars) p)
        d

update' :: Cactus -> String -> Int -> Cactus
update' (Cactus vars Nothing) var val = case (Map.lookup var vars) of
    Nothing -> error "Referenced unbound variable"
    (Just _) -> Cactus (Map.insert var val vars) Nothing
update' (Cactus vars (Just p)) var val = case (Map.lookup var vars) of
    Nothing -> Cactus vars (Just (update' p var val))
    (Just _) -> Cactus (Map.insert var val vars) (Just p)

update :: Scope -> String -> Int -> Scope
update (Scope c d) var val = Scope (update' c var val) d

push :: Scope -> Scope
push (Scope c d) = Scope (Cactus Map.empty (Just c)) d

parent :: Scope -> Scope
parent (Scope (Cactus x Nothing) d) = Scope (Cactus x Nothing) d
parent (Scope (Cactus _ (Just y)) d) = Scope y d

lookup' :: Cactus -> String -> Maybe Int
lookup' (Cactus vars Nothing) var = Map.lookup var vars
lookup' (Cactus vars (Just p)) var = case (Map.lookup var vars) of
    Nothing -> (lookup' p var)
    (Just x) -> (Just x)

lookup :: Scope -> String -> Int
lookup (Scope c Nothing) var = case (lookup' c var) of
    Nothing -> error "Referenced unbound variable"
    (Just x) -> x
lookup (Scope c (Just d)) var = case (lookup' c var) of
    Nothing -> d
    (Just x) -> x

