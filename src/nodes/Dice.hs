module Dice (
    Dice (..),
    fromStr,
    roll,
) where

import System.Random (StdGen, randomR)

data Dice = Dice
    { count :: Int
    , size :: Int
    }
    deriving (Show)

fromStr :: String -> Dice
fromStr s =
    Dice
        { count = (read (takeWhile (/= 'd') s))
        , size = (read (tail (dropWhile (/= 'd') s)))
        }

roll :: StdGen -> Dice -> (Int, StdGen)
roll gen d
    | (count d) <= 0 = (0, gen)
    | (size d) <= 0 = (0, gen)
    | otherwise = roll' (count d) (size d) gen 0
  where
    roll' 0 _ rng acc = (acc, rng)
    roll' c s rng acc =
        let (val, newRng) = randomR (1, s) rng
         in roll' (c - 1) s newRng (acc + val)
