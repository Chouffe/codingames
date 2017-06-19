{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module VoxCodei where

-- import           Control.Arrow
import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes, isJust)
import           Data.Ord            (comparing)
import qualified Data.Set            as S
import qualified Debug.Trace         as T
import           GHC.Generics        (Generic)
import           Safe                (atMay, headMay)
import           System.IO

-- Data Modeling

-- |A cell is either a
-- | S: Surveillance cell
-- | I: Indestructible cell
-- | E: empty cell
data Cell = S | I | E deriving (Eq, NFData, Generic)

instance Show Cell where
    show S = "@"
    show I = "#"
    show E = "."

data Firewall = Firewall {
     _cells  :: [Cell]  -- linearIndex to Cell, should we use a Map Position Cell?
    , height :: Int
    , width  :: Int
    } deriving (Eq, NFData, Generic)

makeFirewall :: [Cell] -> Int -> Int -> Firewall
makeFirewall = Firewall

makeLenses ''Firewall

instance Show Firewall where
    show (Firewall es _ w) = unlines $ showRow <$> makeRows w es
        where
            makeRows :: Int -> [Cell] -> [[Cell]]
            makeRows w' cs = foldl (\rs k -> rs ++ [drop (w' * k) (take (w' * (k + 1)) cs)])
                                   []
                                   [0.. div (length cs) w - 1]

            showRow :: [Cell] -> String
            showRow = concatMap show

newtype Position = Position (Int, Int) deriving (Eq, Ord, NFData, Generic)

position :: Int -> Int -> Position
position x y = Position (x, y)

instance Show Position where
    show (Position (x, y)) = show x ++ " " ++ show y

newtype TTL = TTL Int deriving (Eq, Ord, Show, NFData, Generic)

ttl :: Int -> TTL
ttl x
  | x <= 0 = TTL 0
  | otherwise = TTL x

data Bomb = Bomb Position TTL deriving (Eq, Ord, Show, NFData, Generic)

bomb :: Position -> TTL -> Bomb
bomb = Bomb

data Action = B Bomb | W deriving (Eq, Ord, NFData, Generic)

newtype Range = Range Int deriving (Eq, Show)

range :: Int -> Range
range k
  | k <= 0 = Range 0
  | otherwise = Range k

instance Show Action where
    show (B (Bomb pos _)) = show pos
    show W                = "WAIT"

data Direction = U | D | R | L deriving (Eq, Show)

data GameState = GameState {
    _firewall       :: Firewall
  , _bombs          :: S.Set Bomb
  , _remainingTurns :: Int
  , _remainingBombs :: Int
  } deriving (Eq, Show, NFData, Generic)

makeLenses ''GameState

initGameState :: Firewall -> Int -> Int -> GameState
initGameState f = GameState f S.empty

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

parseInt :: String -> Maybe Int
parseInt str =
  case reads str :: [(Int, String)] of
    [(n, _)] -> Just n
    _        -> Nothing

parseGridSize :: String -> Maybe (Int, Int)
parseGridSize str =
  case words str of
    [h, w] -> liftA2 (,) (parseInt h) (parseInt w)
    _      -> Nothing

parseGameInput :: [String] -> Maybe Firewall
parseGameInput (gridSize:firewallInput) = parseGridSize gridSize >> parseFirewall firewallInput
parseGameInput _ = Nothing

-- Game Logic

-- TODO: write Specs
generateActions :: GameState -> S.Set Action
generateActions gst
  | _remainingTurns gst <= 0 = S.empty
  | _remainingBombs gst <= 0 = S.fromList [W]
  | otherwise                = S.insert W (S.map bombAction (emptyPositions gst))
  where
    -- TODO: cleanup this mess
    emptyPositions :: GameState -> S.Set Position
    emptyPositions g = let f = _firewall g
                           cs = S.fromList $ catMaybes $ zipWith (\i c -> if c == E then Just (linearToCartesian (height f) (width f) i) else Nothing) [0..] $
                               view (firewall . cells) g
                           bs = S.map (\(Bomb pos _) -> pos) (_bombs g)
                       in S.difference cs bs

    bombAction :: Position -> Action
    bombAction pos = B $ bomb pos (ttl 3)

rank :: GameState -> M.Map Action GameTree -> [(Action, GameTree)]
rank gst actionTreeMap = sortByActionDamage gst $ M.assocs actionTreeMap
  where
    sortByActionDamage :: GameState -> [(Action, GameTree)] -> [(Action, GameTree)]
    sortByActionDamage gst' = L.sortBy (flip (comparing (actionDamage gst' . fst)))

prune :: GameState -> [(Action, GameTree)] -> [(Action, GameTree)]
prune = removeNoDamage
  where
    removeNoDamage :: GameState -> [(Action, GameTree)] -> [(Action, GameTree)]
    removeNoDamage gst' = filter (\(a, _) -> actionDamage gst' a >= 0)

heuristic :: GameState -> M.Map Action GameTree -> [(Action, GameTree)]
heuristic gst = prune gst . rank gst

actionDamage :: GameState -> Action -> Int
actionDamage _ W                  = 0
actionDamage gst (B (Bomb pos _)) =
  sum $ map (\d -> bombDirectionDamage (_bombs gst) d (Just pos) (range 4) (_firewall gst)) [U, D, L, R]
  -- TODO: Need to explode current bombs to see what is the next best move
  -- TODO: should return something like State GameState Int instead of an Int... otherwise it wont work
  where
    bombDirectionDamage :: S.Set Bomb -> Direction -> Maybe Position -> Range -> Firewall -> Int
    bombDirectionDamage _ _ _ (Range 0) _         = 0
    bombDirectionDamage _ _ Nothing _ _           = 0
    bombDirectionDamage bs d (Just p) (Range k) f =
      case atMay (_cells f) (cartesianToLinear (height f) (width f) p) of
        Nothing -> 0
        Just I -> 0
        Just E -> case shiftPosition f d p of
                    Nothing -> 0
                    mpos    -> bombDirectionDamage bs d mpos (range (k - 1)) f
        Just S -> case shiftPosition f d p of
                    Nothing -> 1
                    mpos    -> 1 + bombDirectionDamage bs d mpos (range (k - 1)) f

newtype Path = Path [(Action, GameState)] deriving (Eq, Show, NFData, Generic)

emptyPath :: Path
emptyPath = Path []

actionsPath :: Path -> [Action]
actionsPath (Path [])         = []
actionsPath (Path ((a, _):p)) = a : actionsPath (Path p)

gameStatePath :: Path -> [GameState]
gameStatePath (Path [])           = []
gameStatePath (Path ((_, gst):p)) = gst : gameStatePath (Path p)

dfs :: Path -> GameTree -> Maybe Path
dfs (Path p) (Leaf Won) = Just $ Path $ reverse p
dfs _ (Leaf Lost)   = Nothing
dfs (Path p) (Node gst actionTrees) = join $ L.find isJust mpaths

  where
    assocs :: [(Action, GameTree)]
    assocs = heuristic gst actionTrees

    mpaths :: [Maybe Path]
    mpaths = map (\(action, gt) -> dfs (Path ((action, gst):p)) gt) $ T.trace ("heuristic" ++ show (map fst assocs)) assocs

solve :: Int -> Int -> Firewall -> Maybe Path
solve rturns rbombs f = dfs emptyPath (gameTree gameState)
  where
    gameState :: GameState
    gameState = initGameState f rturns rbombs

wait :: GameState -> GameState
wait = id

addBomb :: Bomb -> GameState -> GameState
addBomb b = over bombs (S.insert b) . over remainingBombs (\x -> x - 1)

-- TODO: write Specs
performAction :: Action -> GameState -> GameState
performAction W     = tick . wait
performAction (B b) = tick . addBomb b

-- TODO: write Specs
gameTree :: GameState -> GameTree
gameTree gst
  | won gst   = Leaf Won
  | lost gst  = Leaf Lost
  | otherwise = Node gst (M.fromList [(action, gameTree (performAction action gst)) | action <- S.toList (generateActions gst)])

decreaseTTL :: Bomb -> Bomb
decreaseTTL (Bomb pos (TTL t)) = bomb pos (ttl (t - 1))

-- |It plays one round of the game
tick :: GameState -> GameState
tick gst =
  over bombs (S.map decreaseTTL . S.filter (not . isExplodingBomb)) $
  over remainingTurns (\x -> x - 1) $
  over firewall (explodeBombs (range 3) (S.filter isExplodingBomb (_bombs gst)))
  gst

  where
    isExplodingBomb :: Bomb -> Bool
    isExplodingBomb (Bomb _ (TTL t)) = t <= 0

    explodeBombs :: Range -> S.Set Bomb -> Firewall -> Firewall
    explodeBombs r bs f = S.fold (explode (_bombs gst) r) f bs

-- TODO: write specs
lost :: GameState -> Bool
lost gst = noGameTurns gst || noBombs gst && remainingSurveillanceNodes gst
  -- TODO: how can I write something like the following? -- more elegant
  -- noGameTurns ||| (noBombs &&& remainingSurveillanceNodes)
  where
    noGameTurns :: GameState -> Bool
    noGameTurns = (<=0) . _remainingTurns

    noBombs :: GameState -> Bool
    noBombs gst' = null (_bombs gst') && _remainingBombs gst' == 0

    remainingSurveillanceNodes :: GameState -> Bool
    remainingSurveillanceNodes = elem S . view (firewall . cells)

-- TODO: write specs
won :: GameState -> Bool
won gst = noSurveillanceNodes gst && remaingGameTurns gst
  where
    remaingGameTurns :: GameState -> Bool
    remaingGameTurns = (>=0) . _remainingTurns

    noSurveillanceNodes :: GameState -> Bool
    noSurveillanceNodes = notElem S . view (firewall . cells)

explode :: S.Set Bomb -> Range -> Bomb -> Firewall -> Firewall
explode bs r b@(Bomb pos (TTL t))
  | t > 0 = id
  | otherwise = forkExplode bombs'

    where
      bombs' :: S.Set Bomb
      bombs' = S.delete b bs

      -- TODO use a fold here instead
      forkExplode :: S.Set Bomb -> Firewall -> Firewall
      forkExplode bs' f =
        explodeDirection bs' L (shiftPosition f L pos) r $
        explodeDirection bs' R (shiftPosition f R pos) r $
        explodeDirection bs' U (shiftPosition f U pos) r $
        explodeDirection bs' D (shiftPosition f D pos) r f

explodeDirection :: S.Set Bomb -> Direction -> Maybe Position -> Range -> Firewall -> Firewall
explodeDirection _ _ _ (Range 0) f         = f
explodeDirection _ _ Nothing _ f           = f
explodeDirection bs d (Just p) (Range k) f =
  case atMay (_cells f) (cartesianToLinear (height f) (width f) p) of
    Nothing -> f
    Just I -> f
    Just E -> case shiftPosition f d p of
                 Nothing -> newFirewall
                 pos'    -> explodeDirection bs d pos' (range (k - 1)) newFirewall
              where newFirewall = case L.find (\(Bomb p' _) -> p' == p) bs of
                                    -- Recursively explode bombs
                                    Just b@(Bomb pos _) -> explode (S.delete b bs) (range 3) (bomb pos (ttl 0)) f
                                    Nothing             -> f
    Just S -> case shiftPosition f d p of
                Nothing -> newFirewall
                pos'    -> explodeDirection bs d pos' (range (k - 1)) newFirewall
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

-- IO Actions

gameInput :: IO (Maybe Firewall)
gameInput = do
  gridSizeInput <- getLine
  case parseGridSize gridSizeInput of
    Nothing -> return Nothing
    Just (h, _) -> do
      firewallInput <- replicateM h getLine
      case parseFirewall firewallInput of
        Nothing -> return Nothing
        mf      -> return mf

gameLoopInput :: IO (Maybe (Int, Int))
gameLoopInput = do
  remaining <- getLine
  case words remaining of
    [rturns, rbombs] -> return $ liftA2 (,) (parseInt rturns) (parseInt rbombs)
    _ -> return Nothing

gameLoop :: GameState -> IO ()
gameLoop gst = do
  mstate <- gameLoopInput
  case mstate of
    Nothing -> return ()
    Just (rturns, rbombs) -> do
      debugGameLoop rturns rbombs
      case mpath of
        Nothing -> return ()
        Just (Path []) -> return ()
        Just p@(Path ((a,_):_)) -> do
          debugPath p
          print a
          gameLoop (performAction a gst)
      where mpath = solve rturns rbombs (_firewall gst)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    mfirewall <- gameInput
    case mfirewall of
      Nothing -> return ()
      Just f  -> debugMain f >> gameLoop (initGameState f 0 0)

-- Debug

debugGameLoop :: Int -> Int -> IO ()
debugGameLoop rturns rbombs = do
  hPutStrLn stderr $ "Turns remaining: " ++ show rturns
  hPutStrLn stderr $ "Bombs remaining: " ++ show rbombs

debugMain :: Firewall -> IO ()
debugMain f = hPutStrLn stderr $ "Firewall: " ++ show f

debugPath :: Path -> IO ()
debugPath p = hPutStrLn stderr $ "actions foreseen: " ++ show (actionsPath p)

-- Tests

firewall1 :: Firewall
firewall1 = makeFirewall [ E, E, E, E
                         , E, S, E, E
                         , E, E, E, E
                         ] 3 4

firewall2 :: Firewall
firewall2 = makeFirewall [ E, E, E, E
                         , S, E, S, S
                         , E, E, E, E
                         ] 3 4

firewall4 :: Firewall
firewall4 = makeFirewall [ E, S, E, E
                         , S, E, S, E
                         , E, S, E, E
                         ] 3 4

firewall5 :: Firewall
firewall5 = makeFirewall [ E, E, E, E, S, S, S, E
                         , E, S, S, S, E, E, E, S
                         , S, E, E, E, S, E, E, S
                         , S, E, E, E, S, E, E, S
                         , S, E, E, E, S, E, E, E
                         , E, S, S, S, E, S, S, S
                         ] 6 8

firewall6 :: Firewall
firewall6 = makeFirewall [ E, S, E, E, E, E, E, S
                         , E, E, E, E, E, E, E, E
                         , E, E, E, E, E, E, E, E
                         , E, E, E, E, E, E, E, E
                         , E, E, E, E, E, E, E, E
                         , E, S, E, E, E, E, E, S
                         ] 6 8

firewall10 :: Firewall
firewall10 = makeFirewall [ E, E, E, E, E, E, E, E, E, E, E, E
                          , E, I, S, S, S, E, I, S, S, S, E, E
                          , E, S, E, E, E, E, S, E, E, E, E, E
                          , E, S, E, E, E, E, S, E, E, E, E, E
                          , E, S, E, E, E, E, S, E, E, S, I, E
                          , E, S, E, E, E, E, S, E, E, E, S, E
                          , E, E, S, S, S, E, E, S, S, S, I, E
                          , E, E, E, E, E, E, E, E, E, E, E, E
                          , E, E, E, E, E, E, E, E, E, E, E, E
                          ] 9 12

gamestate1 :: GameState
gamestate1 = initGameState firewall1 5 1

gamestate2 :: GameState
gamestate2 = initGameState firewall2 5 1

gamestate4 :: GameState
gamestate4 = initGameState firewall4 5 1

gamestate5 :: GameState
gamestate5 = initGameState firewall5 8 3

gamestate6 :: GameState
gamestate6 = initGameState firewall6 8 2

bomb1 :: Bomb
bomb1 = bomb (position 0 0) (ttl 3)

gameTree1 :: GameTree
gameTree1 = gameTree gamestate1

gameTree2 :: GameTree
gameTree2 = gameTree gamestate2

gameTree4 :: GameTree
gameTree4 = gameTree gamestate4

gameTree5 :: GameTree
gameTree5 = gameTree gamestate5

gameTree6 :: GameTree
gameTree6 = gameTree gamestate6
