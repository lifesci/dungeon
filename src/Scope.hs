module Scope (
    Scope,
    empty,
    add,
    update,
    push,
    parent,
    lookup,
    lookupWithDefault
) where

import Prelude hiding (lookup)
import Data.Map(Map)
import qualified Data.Map as Map

data Scope = Scope (Map String Int) (Maybe Scope) deriving Show

empty :: Scope
empty = (Scope Map.empty Nothing)

add :: Scope -> String -> Int -> Scope
add (Scope vars p) var val = Scope (Map.insert var val vars) p

update :: Scope -> String -> Int -> Scope
update (Scope vars Nothing) var val = case (Map.lookup var vars) of
    Nothing -> error "Referenced unbound variable"
    (Just _) -> Scope (Map.insert var val vars) Nothing
update (Scope vars (Just p)) var val = case (Map.lookup var vars) of
    Nothing -> Scope vars (Just (update p var val))
    (Just _) -> Scope (Map.insert var val vars) (Just p)

push :: Scope -> Scope
push x = (Scope Map.empty (Just x))

parent :: Scope -> Scope
parent (Scope x Nothing) = Scope x Nothing
parent (Scope _ (Just y)) = y

lookup' :: Scope -> String -> Maybe Int
lookup' (Scope vars Nothing) var = Map.lookup var vars
lookup' (Scope vars (Just p)) var = case (Map.lookup var vars) of
    Nothing -> (lookup' p var)
    (Just x) -> (Just x)

lookup :: Scope -> String -> Int
lookup scp var = case (lookup' scp var) of
    Nothing -> error "Referenced unbound variable"
    (Just x) -> x

lookupWithDefault :: Scope -> String -> Int -> Int
lookupWithDefault scp var def = case (lookup' scp var) of
    Nothing -> def
    (Just x) -> x

