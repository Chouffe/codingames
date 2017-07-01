{-# LANGUAGE ScopedTypeVariables #-}

module PowerOfThor where

import           System.IO

-- Data Modeling
data Dir = N | NE | E | SE | S | SW | W | NW deriving (Read, Eq, Show)
data Position = P { x :: Int, y :: Int } deriving (Eq, Show)

-- Parsers

parseGameInput :: String -> Maybe (Position, Position)
parseGameInput s =
  case words s of
    [lx, ly, tx, ty] -> Just (P (read lx) (read ly), P (read tx) (read ty))
    _                -> Nothing

-- Game Logic

-- |nextMove from to = dir, newFrom
nextMove :: Position -> Position -> Maybe (Dir, Position)
nextMove (P x1 y1) (P x2 y2) = do
  dir <- optDir dx dy
  return (dir, P (x1 + dx) (y1 + dy))
  where
    (dx, dy) = (signum (x2 - x1), signum (y2 - y1))

    optDir :: Int -> Int -> Maybe Dir
    optDir dx' dy' = case (dx', dy') of
                       (-1, -1) -> Just NW
                       (-1, 0)  -> Just W
                       (-1, 1)  -> Just SW
                       (0, -1)  -> Just N
                       (0, 0)   -> Nothing
                       (0, 1)   -> Just S
                       (1, -1)  -> Just NE
                       (1, 0)   -> Just E
                       (1, 1)   -> Just SE
                       _        -> Nothing


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    mpos <- fmap parseGameInput getLine
    case mpos of
      Nothing         -> fail "Could not parse"
      Just (to, from) -> loop from to

loop :: Position -> Position -> IO ()
loop from to = do
  debugInput from to
  _ <- getLine
  case nextMove from to of
    Nothing             -> fail "Could not find nextMove"
    Just (dir, newFrom) -> putStrLn (show dir) >> loop newFrom to

-- Debug

debugInput :: Position -> Position -> IO ()
debugInput from to = do
  hPutStrLn stderr $ "from: " ++ show from
  hPutStrLn stderr $ "to: " ++ show to
