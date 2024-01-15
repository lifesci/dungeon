module Lib (
    rev,
    listToMap,
    join,
    applyTabs
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

tab :: Int -> String
tab n
    | n <= 0 = []
    | otherwise = ' ':(tab (n-1))

applyTabs :: [String] -> Int  -> [String]
applyTabs sl t = map (\s -> (tab t) ++ s) sl

join :: String -> [String] -> String
join _ [] = ""
join _ (x:[]) = x
join i (x:xs) = x ++ i ++ (join i xs)

