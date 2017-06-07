{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module VoxCodei where

-- import           Control.Arrow
import           Control.Lens
import           Control.Monad.Trans.State.Strict
import qualified Data.List                        as L
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import           Safe                             (atMay, headMay)


-- import qualified Data.List as L

-- Data Modeling

-- |A cell is either a surveillance cell, and indestructible cell or empty
data Cell = S | I | E deriving (Eq)

instance Show Cell where
    show S = "@"
    show I = "#"
    show E = "."

data Firewall = Firewall {
     _cells  :: [Cell]
    , height :: Int
    , width  :: Int
    } deriving (Eq)

makeLenses ''Firewall

instance Show Firewall where
    show (Firewall es _ w) = unlines $ showRow <$> (makeRows w es)
        where
            makeRows :: Int -> [Cell] -> [[Cell]]
            makeRows w' cs = foldl (\rs k -> rs ++ [drop (w' * k) (take (w' * (k + 1)) cs)])
                                   []
                                   [0..(div (length cs) w) - 1]

            showRow :: [Cell] -> String
            showRow = concat . fmap show

newtype Position = Position (Int, Int) deriving (Eq, Ord)

instance Show Position where
    show (Position (x, y)) = show x ++ " " ++ show y

newtype TTL = TTL Int deriving (Eq, Ord, Show)

data Bomb = Bomb Position TTL deriving (Eq, Ord, Show)

data Action = B Bomb | W deriving (Eq, Ord)

newtype Range = Range Int deriving (Eq, Show)

instance Show Action where
    show (B (Bomb pos _)) = show pos
    show W                = "WAIT"

data Direction = U | D | R | L deriving (Eq, Show)

data GameState = GameState {
    _firewall       :: Firewall
  , _bombs          :: [Bomb]  -- Use a set for efficient lookups?
  , _remainingTurns :: Int
  , _remaingBombs   :: Int
  } deriving (Eq, Show)

makeLenses ''GameState

data Finished = Lost | Won deriving (Eq, Show)

data GameTree = Leaf Finished
              | Node GameState (M.Map Action GameTree)
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
    cs <- traverse parseCell (concat xs)
    return $ Firewall cs (length xs) (length s)

parseCell :: Char -> Maybe Cell
parseCell '@' = Just S
parseCell '#' = Just I
parseCell '.' = Just E
parseCell _   = Nothing

-- Game Logic

-- TODO
-- TODO: write Specs
generateActions :: GameState -> S.Set Action
generateActions gst = S.union (S.fromList (fmap bombAction (emptyPositions gst)))
                              (S.fromList [W])
  where
    emptyPositions :: GameState -> [Position]
    emptyPositions _ = [] -- TODO: filter out bombs

    bombAction :: Position -> Action
    bombAction pos = B $ Bomb pos (TTL 3)

-- TODO
rank :: GameState -> S.Set Action -> [Action]
rank = const S.toList

-- TODO: Implement some greedy heuristic to pick the best Bomb positions

newtype Path = Path [Action] deriving (Eq, Show)

isWinningPath :: Path -> Bool
isWinningPath = undefined

data TreeSearchState = TSS { path :: Path }
  deriving (Eq, Show)

-- TODO
dfs :: GameTree -> State TreeSearchState ()
dfs = undefined

-- TODO: write Specs
wait :: GameState -> GameState
wait = over remainingTurns (\x -> x - 1)

-- TODO: write Specs
performAction :: Action -> GameState -> GameState
performAction W        = tick . wait
performAction (B bomb) = tick . over bombs (bomb:)

-- TODO: write Specs
gameTree :: GameState -> GameTree
gameTree gst
  | lost gst  = Leaf Lost
  | won gst   = Leaf Won
  | otherwise = Node gst (M.fromList [(action, gameTree (performAction action gst)) | action <- S.toList (generateActions gst)])

-- TODO: write Specs
-- |It plays one round of the game
tick :: GameState -> GameState
tick gst =
  over bombs ((filter (not . isExplodingBomb)) . map decreaseTTL) $
  over remainingTurns (\x -> x - 1) $
  over firewall (explodeBombs (Range 3) (filter isExplodingBomb (_bombs gst))) $
  gst

  where
    isExplodingBomb :: Bomb -> Bool
    isExplodingBomb (Bomb _ (TTL ttl)) = ttl <= 0

    decreaseTTL :: Bomb -> Bomb
    decreaseTTL (Bomb pos (TTL ttl)) = Bomb pos (TTL (ttl - 1))

    explodeBombs :: Range -> [Bomb] -> Firewall -> Firewall
    explodeBombs r bs f = foldl (\f' b -> explode (_bombs gst) r b f') f bs

-- TODO: write specs
lost :: GameState -> Bool
lost gst = (noGameTurns gst) || (noBombs gst && remainingSurveillanceNodes gst)
  -- TODO: how can I write something like the following? -- more elegant
  -- noGameTurns ||| (noBombs &&& remainingSurveillanceNodes)
  where
    noGameTurns :: GameState -> Bool
    noGameTurns = (<=0) . _remainingTurns

    noBombs :: GameState -> Bool
    noBombs gst' = length (_bombs gst') == 0 && _remaingBombs gst' == 0

    remainingSurveillanceNodes :: GameState -> Bool
    remainingSurveillanceNodes = any (==S) . view (firewall . cells)

-- TODO: write specs
won :: GameState -> Bool
won gst = (noSurveillanceNodes gst) && (remaingGameTurns gst)
  where
    remaingGameTurns :: GameState -> Bool
    remaingGameTurns = (>=0) . _remainingTurns

    noSurveillanceNodes :: GameState -> Bool
    noSurveillanceNodes = all (/=S) . view (firewall . cells)

-- TODO: write specs
explode :: [Bomb] -> Range -> Bomb -> Firewall -> Firewall
explode bs range b@(Bomb pos (TTL ttl))
  | ttl > 0 = id
  | otherwise = forkExplode bombs'

    where
      bombs' :: [Bomb]
      bombs' = filter (/= b) bs

      forkExplode :: [Bomb] -> Firewall -> Firewall
      forkExplode bs' f =
        explodeDirection bs' L (shiftPosition f L pos) range $
        explodeDirection bs' R (shiftPosition f R pos) range $
        explodeDirection bs' U (shiftPosition f U pos) range $
        explodeDirection bs' D (shiftPosition f D pos) range $ f

explodeDirection :: [Bomb] -> Direction -> (Maybe Position) -> Range -> Firewall -> Firewall
explodeDirection _ _ _ (Range 0) f = f
explodeDirection _ _ Nothing _ f = f
explodeDirection bs d (Just p) (Range k) f =
  case atMay (_cells f) (cartesianToLinear (height f) (width f) p) of
    Nothing -> f
    Just I -> f
    Just E -> case (shiftPosition f d p) of
                 Nothing -> newFirewall
                 pos'    -> explodeDirection bs d pos' (Range (k - 1)) newFirewall
              where newFirewall = case L.find (\(Bomb p' _) -> p' == p) bs of
                                    -- Recursively explode bombs
                                    Just b -> explode (filter (/= b) bs) (Range 3) b f
                                    Nothing -> f
    Just S -> case shiftPosition f d p of
                Nothing -> newFirewall
                pos'    -> explodeDirection bs d pos' (Range (k - 1)) newFirewall
              where newFirewall = deleteSurveillanceNodeAt p f

shiftPosition :: Firewall -> Direction -> Position -> Maybe Position
shiftPosition _ L (Position (x, y))
  | x <= 0 = Nothing
  | otherwise = Just $ Position (x - 1, y)
shiftPosition f R (Position (x, y))
  | x >= (width f - 1) = Nothing
  | otherwise = Just $ Position (x + 1, y)
shiftPosition _ U (Position (x, y))
  | y <= 0 = Nothing
  | otherwise = Just $ Position (x, y - 1)
shiftPosition f D (Position (x, y))
  | y >= (height f - 1) = Nothing
  | otherwise = Just $ Position (x, y + 1)

deleteSurveillanceNodeAt :: Position -> Firewall -> Firewall
deleteSurveillanceNodeAt pos f = over cells updateCells f
  where
    k :: Int
    k = cartesianToLinear (height f) (width f) pos

    updateAt :: Int -> Int -> Cell -> Cell
    updateAt l i c = case (i == l, c) of
                        (True, S) -> E
                        _         -> c

    updateCells :: [Cell] -> [Cell]
    updateCells = zipWith (updateAt k) [0..]
