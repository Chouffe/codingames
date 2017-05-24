{-# LANGUAGE ScopedTypeVariables #-}

module ShadowsOfTheKnight where

import           System.IO

-- Data Modeling

newtype Position = Position (Int, Int) deriving (Eq)

instance Show Position where
    show (Position (x, y)) = show x ++ " " ++ show y

data Board = Board { width  :: Int
                   , height :: Int
                   , start  :: Position
                   , end    :: Position
                   } deriving (Eq, Show)

initBoard :: Int -> Int -> Board
initBoard w h = Board w h (Position (0, 0)) (Position (w - 1, h - 1))

data Direction = U | UR | R | DR | D | DL | L | UL
    deriving (Eq, Show, Read)

-- Parsers

parseBoard :: String -> Maybe Board
parseBoard boardDimensions =
    case (fmap reads (words boardDimensions)) :: [[(Int, String)]] of
      [[(w, _)], [(h, _)]] -> Just $ initBoard w h
      _                    -> Nothing

parsePosition :: String -> Maybe Position
parsePosition initialPosition =
    case (fmap reads (words initialPosition)) :: [[(Int, String)]] of
      [[(x0, _)], [(y0, _)]] -> Just $ Position (x0, y0)
      _                      -> Nothing

parseGameInput :: (String, String, String) ->  Maybe (Board, Position)
parseGameInput (boardDimensions, _, initialPosition) =
    (,) <$> parseBoard boardDimensions <*> parsePosition initialPosition

parseGameLoopInput :: String -> Maybe Direction
parseGameLoopInput dir = case reads dir :: [(Direction, String)] of
                           [(direction, _)] -> Just direction
                           _                -> Nothing

-- Game logic

dichotomie :: Board -> Position
dichotomie board = let Position (x0, y0) = start board
                       Position (x1, y1) = end board
                   in Position ((x0 + x1) `div` 2, (y0 + y1) `div` 2)

nextBoard :: Board -> Position -> Direction -> Board
nextBoard (Board w h (Position (x0, y0)) (Position (x1, y1))) (Position (u, v)) dir =
    case dir of
      U  -> Board w h (Position (u, y0))        (Position (u, v - 1))
      UR -> Board w h (Position (u + 1, y0))    (Position (x1, v - 1))
      R  -> Board w h (Position (u + 1, v))     (Position (x1, v))
      DR -> Board w h (Position (u + 1, v + 1)) (Position (x1, y1))
      D  -> Board w h (Position (u, v + 1))     (Position (u, y1))
      DL -> Board w h (Position (x0, v + 1))    (Position (u - 1, y1))
      L  -> Board w h (Position (x0, v))        (Position (u - 1, v))
      UL -> Board w h (Position (x0, y0))       (Position (u - 1, v - 1))

-- Game IO actions

debugGameLoop :: Board -> Position -> IO ()
debugGameLoop board position = do
    hPutStrLn stderr (show board)
    hPutStrLn stderr (show position)

gameLoop :: Board -> Position -> IO ()
gameLoop board position = do
    debugGameLoop board position
    mdirection <- parseGameLoopInput <$> getLine
    case mdirection of
      Nothing -> return ()  -- Parsing failed
      Just direction -> do
          let newBoard = nextBoard board position direction
              newPosition = dichotomie newBoard
          putStrLn $ show newPosition
          gameLoop newBoard newPosition

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    gameInput <- (,,) <$> getLine <*> getLine <*> getLine
    case parseGameInput gameInput of
      Just (board, position) -> gameLoop board position  -- Run game loop
      Nothing                -> return ()                -- Parsing failed
