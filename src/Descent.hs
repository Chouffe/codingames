module Descent where

import           Control.Monad
import           Data.Foldable
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- The while loop represents the game.
    -- Each iteration represents a turn of the game
    -- where you are given inputs (the heights of the mountains)
    -- and where you have to print an output (the index of the mountain to fire on)
    -- The inputs you are given are automatically updated according to your last actions.

    loop

mountainHeights :: Int -> IO [Int]
mountainHeights n = replicateM n $ read <$> getLine

maximumIndex :: (Ord a) => [a] -> Int
maximumIndex xs = snd $ maximumBy (\a b -> compare (fst a) (fst b)) $ zip xs [0..]

loop :: IO ()
loop = do
    xs <- mountainHeights 8
    putStrLn $ show $ maximumIndex xs
    loop
