module Lib (
    rev
) where

rev :: [a] -> [a]
rev xs = foldl rev' [] xs
    where
        rev' :: [a] -> a -> [a]
        rev' acc x = x:acc
