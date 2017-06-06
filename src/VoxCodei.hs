{-# LANGUAGE ScopedTypeVariables #-}

module VoxCodei where

import qualified Data.List as L
import qualified Data.Map  as M
import           Safe      (atMay, headMay)

-- import qualified Data.List as L

-- Data Modeling

-- |A cell is either a surveillance cell, and indestructible cell or empty
data Cell = S | I | E deriving (Eq)
-- TODO: should we allow bombs on a cell?

instance Show Cell where
    show S = "@"
    show I = "#"
    show E = "."

data Firewall = Firewall {
     _cells  :: [Cell]
    , height :: Int
    , width  :: Int
    } deriving (Eq)


instance Show Firewall where
    show (Firewall cells _ w) = unlines $ showRow <$> (makeRows w cells)
        where
            makeRows :: Int -> [Cell] -> [[Cell]]
            makeRows w' cs = foldl (\rs k -> rs ++ [drop (w' * k) (take (w' * (k + 1)) cs)])
                                   []
                                   [0..(div (length cs) w) - 1]

            showRow :: [Cell] -> String
            showRow = concat . fmap show

newtype Position = Position (Int, Int) deriving (Eq)

instance Show Position where
    show (Position (x, y)) = show x ++ " " ++ show y

newtype TTL = TTL Int deriving (Eq, Show)

data Bomb = Bomb Position TTL deriving (Eq, Show)

data Action = B Bomb | W deriving (Eq)

newtype Range = Range Int deriving (Eq, Show)

instance Show Action where
    show (B (Bomb pos _)) = show pos
    show W                = "WAIT"

data Direction = U | D | R | L deriving (Eq, Show)

data GameState = GameState {
    _firewall      :: Firewall
  , _bombs         :: [Bomb]  -- Use a set for efficient lookups?
  , remainingTurns :: Int
  , remaingBombs   :: Int
  } deriving (Eq, Show)

data Finished = Lost | Won deriving (Eq, Show)

data GameTree = Leaf Finished
              | Node GameState (M.Map Action GameState)
              deriving (Eq, Show)

cartesianToLinear :: Int -> Int -> Position -> Int
cartesianToLinear _ w (Position (x, y)) = y * w + x

linearToCartesian :: Int -> Int -> Int -> Position
linearToCartesian _ w l = Position (x', y')
  where (y', x') = divMod l w

-- Parsers

parseFirewall :: [String] -> Maybe Firewall
parseFirewall xs = do
    s <- headMay xs
    cells <- traverse parseCell (concat xs)
    return $ Firewall cells (length xs) (length s)

parseCell :: Char -> Maybe Cell
parseCell '@' = Just S
parseCell '#' = Just I
parseCell '.' = Just E
parseCell _   = Nothing

-- Game Logic

-- TODO
tick :: GameState -> GameState
tick = id

-- TODO: write specs
explode :: [Bomb] -> Range -> Bomb -> Firewall -> Firewall
explode bombs range b@(Bomb pos (TTL ttl))
  | ttl > 0 = id
  | otherwise =
      explodeDirection bombs' L (shiftPosition pos L) range .
      explodeDirection bombs' R (shiftPosition pos R) range .
      explodeDirection bombs' U (shiftPosition pos U) range .
      explodeDirection bombs' D (shiftPosition pos D) range
    where bombs' = filter (/= b) bombs

explodeDirection :: [Bomb] -> Direction -> (Maybe Position) -> Range -> Firewall -> Firewall
explodeDirection _ _ _ (Range 0) f = f
explodeDirection _ _ Nothing _ f = f
explodeDirection bs d (Just p) (Range k) firewall =
  case atMay (_cells firewall) (cartesianToLinear (height firewall) (width firewall) p) of
    Nothing -> firewall
    Just I -> firewall
    Just E -> case (shiftPosition p d) of
                 Nothing -> newFirewall
                 pos'    -> explodeDirection bs d pos' (Range (k - 1)) newFirewall
              where newFirewall = case L.find (\(Bomb p' _) -> p' == p) bs of
                                    -- Recursively explode bombs
                                    Just b -> explode (filter (/= b) bs) (Range 3) b firewall
                                    Nothing -> firewall
    Just S -> case shiftPosition p d of
                Nothing -> newFirewall
                pos'    -> explodeDirection bs d pos' (Range (k - 1)) newFirewall
              where newFirewall = deleteSurveillanceNodeAt p firewall

-- TODO
shiftPosition :: Position -> Direction -> Maybe Position
shiftPosition pos d = Nothing

-- TODO
deleteSurveillanceNodeAt :: Position -> Firewall -> Firewall
deleteSurveillanceNodeAt pos = id
