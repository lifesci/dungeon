module Lib (
    rev,
    listToMap,
    join,
    applyTab,
    applyTabs,
    split,
    popMap
) where

import Data.Map(Map)
import qualified Data.Map as Map

rev :: [a] -> [a]
rev xs = foldl rev' [] xs
    where
        rev' :: [a] -> a -> [a]
        rev' acc x = x:acc

listToMap :: Ord b => [a] -> (a -> b) -> (a -> c) -> Map b c
listToMap xs key val = foldr (\x acc -> (Map.insert (key x) (val x) acc)) Map.empty xs

popMap :: Ord a => a -> Map a b -> (Maybe b, Map a b)
popMap key m = (Map.lookup key m, Map.filterWithKey (\k _ -> k /= key) m)

tab :: Int -> String
tab n
    | n <= 0 = []
    | otherwise = ' ':(tab (n-1))

applyTabs :: [String] -> Int  -> [String]
applyTabs sl t = map (\s -> applyTab s t) sl

applyTab :: String -> Int  -> String
applyTab s t = (tab t) ++ s

join :: String -> [String] -> String
join _ [] = ""
join _ (x:[]) = x
join i (x:xs) = x ++ i ++ (join i xs)

splitTake :: Char -> String -> (String, String)
splitTake char str = splitTake' char str [] where
    splitTake' _ [] acc = (acc, [])
    splitTake' c (x:xs) acc = if x == c then (acc, xs) else splitTake' c xs (x:acc)

split :: Char -> String -> [String]
split _ [] = []
split c s =
    let
        (part, rest) = splitTake c s
    in
        if part == "" then split c rest else (rev part):(split c rest)

