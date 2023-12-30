module Scope where

import Data.Map(Map)
import qualified Data.Map as Map

data Scope = Scope (Map String Int) (Maybe Scope) deriving Show

empty :: Scope
empty = (Scope Map.empty Nothing)

add :: Scope -> String -> Int -> Scope
add (Scope vars parent) var val = Scope (Map.insert var val vars) parent

update :: Scope -> String -> Int -> Scope
update (Scope vars Nothing) var val = case (Map.lookup var vars) of
    Nothing -> error "Referenced unbound variable"
    (Just x) -> Scope (Map.insert var val vars) Nothing
update (Scope vars (Just parent)) var val = case (Map.lookup var vars) of
    Nothing -> Scope vars (Just (update parent var val))
    (Just x) -> Scope (Map.insert var val vars) (Just parent)

push :: Scope -> Scope
push x = (Scope Map.empty (Just x))

parent :: Scope -> Scope
parent (Scope x Nothing) = Scope x Nothing
parent (Scope _ (Just y)) = y

search :: Scope -> String -> Int
search (Scope vars Nothing) var = case (Map.lookup var vars) of
    Nothing -> error "Referenced unbound variable"
    (Just x) -> x
search (Scope vars (Just parent)) var = case (Map.lookup var vars) of
    Nothing -> (search parent var)
    (Just x) -> x
