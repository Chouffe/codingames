{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module SkynetRevolution2 where

import           Control.Applicative              ((<|>))
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Function                    (on)
import           Data.List                        (delete, find, group, groupBy,
                                                   nub, null, sort, sortBy)
import           Data.Map                         (Map, assocs, empty, fromList,
                                                   fromListWith, insert, lookup,
                                                   updateWithKey)
import qualified Data.Map                         as M
import           Data.Maybe                       (catMaybes, fromJust, isJust)
import           Data.Ord                         (comparing)
import           Data.Set                         (Set, fromList, insert,
                                                   intersection, member, toList)
import qualified Data.Set                         as S
-- import           Debug.Trace                      (trace)
import           System.IO

-- utils

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast = safeHead . reverse

sortAndGroup :: Ord a => [(a, b)] -> Map a [b]
sortAndGroup as = fromListWith (++) [(k, [v]) | (k, v) <- as]

-- Data Modeling

type Vertex = Int
type Gateway = Vertex
type Position = Vertex
type Link = (Vertex, Vertex)
type Path = [Vertex]
type Graph = Map Vertex [Vertex]

data GraphSearchState = GSS
  { mvisited   :: Set Vertex
  , mfrontiers :: [Path]
  , mpaths     :: [Path]
  } deriving (Eq, Show)

data Turn = Player | Agent deriving (Eq, Show)
data GameState = Lost | Won deriving (Eq, Show)
data Action = Move Position | Sever Link deriving (Eq, Ord, Show)
data World = World
  { mgateways :: Set Gateway
  , mgraph    :: Graph
  , magent    :: Position
  }
  deriving (Eq, Show)

data GameTree = Leaf World GameState
              | Node Turn World (Map Action GameTree)
              deriving (Eq, Show)

nextTurn :: Turn -> Turn
nextTurn Player = Agent
nextTurn Agent  = Player

remainingSeparableLinks :: World -> [Link]
remainingSeparableLinks world = do
  g <- Data.Set.toList $ mgateways world
  let cs = case children (mgraph world) g of
              Nothing -> []
              Just xs -> xs
  c <- cs
  return $ (g, c)

playerWon :: World -> Bool
playerWon world = Data.List.null $ remainingSeparableLinks world

playerLost :: World -> Bool
playerLost world = (magent world) `elem` (mgateways world)

generateAgentActions :: World -> [Action]
generateAgentActions world =
  case children (mgraph world) (magent world) of
    Nothing -> []
    Just xs -> (fmap Move xs)

generatePlayerActions :: World -> [Action]
generatePlayerActions world = fmap Sever $ remainingSeparableLinks world

performAction :: Action -> World -> World
performAction (Move to)    world = World (mgateways world) (mgraph world) to
performAction (Sever link) world = World (mgateways world) (severLink (mgraph world) link) (magent world)

generateGameTree :: Turn -> World -> GameTree
generateGameTree turn world
  | playerWon world  = Leaf world Won
  | playerLost world = Leaf world Lost
  | otherwise        =
    Node turn world $ generateChildren (case turn of
                                          Player -> generatePlayerActions
                                          Agent  -> generateAgentActions)
  where
    generateChildren getActions =
      Data.Map.fromList $
        fmap (\action -> (action, generateGameTree (nextTurn turn) (performAction action world)))
        (getActions world)

safeGameTree :: Int -> GameTree -> Bool
safeGameTree 0 (Node _ _ _)                        = True
safeGameTree _ (Leaf _ Won)                        = True
safeGameTree _ (Leaf _ Lost)                       = False
safeGameTree depth (Node Player world gameTreeMap) = any (safeGameTree (depth - 1)) $ fmap snd $ playerPolicy world gameTreeMap
safeGameTree depth (Node Agent world gameTreeMap)  = all (safeGameTree (depth - 1)) $ fmap snd $ agentPolicy 5 world gameTreeMap
-- safeGameTree n (Node Agent _ gameTreeMap)      = all (safeGameTree (n - 1)) $ elems gameTreeMap

pathToLink :: Path -> Maybe Link
pathToLink path
  | n < 2 = Nothing
  | otherwise = case drop (n - 2) path of
                  [i, o] -> Just (i, o)
                  _      -> Nothing
  where n = length path

utility :: GameTree -> Double
utility (Leaf _ Won)  = 0.0
utility (Leaf _ Lost) = 100.0
utility (Node _ world _) = sum $ map (\grp@(path:_) -> (fromIntegral (length grp)) / (fromIntegral (1 + length path))) groupedPaths
  where
    paths = mpaths (runWorldBfs world)
    groupedPaths = groupBy ((==) `on` length) $ sortBy (comparing length) paths

agentPolicy :: Int -> World -> Map Action GameTree -> [(Action, GameTree)]
agentPolicy k _ gameTreeMap = take k $ reverse $ sortBy (comparing (utility . snd)) $ assocs gameTreeMap

-- TODO: improve: not readable and really hard to compose... :(
playerPolicy :: World -> Map Action GameTree -> [(Action, GameTree)]
playerPolicy world gameTreeMap = case msortedActions of
                                   Nothing -> assocs gameTreeMap
                                   Just xs -> nub (xs ++ assocs gameTreeMap)
  where paths = mpaths (runWorldBfs world)
        values = fmap (\path -> (length path, safeLast (reverse (drop 1 (reverse path))), pathToLink path)) paths
        values' = fmap (\(a,b,c) -> (a, fromJust b, fromJust c)) $ filter (\(_,b,c) -> isJust b && isJust c) values
        values'' = sortBy (\(x1,y1,_) (x2,y2,_) -> compare (x1, y1) (x2, y2)) values'
        groupedValues = groupBy (\(x1, y1, _) (x2, y2, _) -> (x1, y1) == (x2, y2)) values''
        frequencies = fmap (\grp -> (length grp, grp)) groupedValues
        sortedDescFrequencies = sortBy (\a b -> compare (fst a) (fst b)) frequencies
        sortedActions = nub [Sever link | (_, grp) <- sortedDescFrequencies, (_, _, link) <- grp]
        msortedActions = traverse (\a -> case Data.Map.lookup a gameTreeMap of
                                           Nothing -> Nothing
                                           Just x  -> Just (a, x)) sortedActions
pickPlayerAction :: Int -> GameTree -> Maybe Action
pickPlayerAction depth (Node Player world gameTreeMap) =
  fmap fst $ find snd $ fmap (fmap (safeGameTree depth)) $ playerPolicy world gameTreeMap
pickPlayerAction _ _ = Nothing

-- Graph API

initGraph :: Graph
initGraph = empty

makeGraph :: [Link] -> Graph
makeGraph links =
    foldr (\ls graph -> case ls of
                          []         -> graph
                          ((i, _):_) -> Data.Map.insert i (fmap snd ls) graph
          ) initGraph $
            groupBy ((==) `on` fst) $
            sortBy (comparing fst) $
            nub $
            links >>= \(i, o) -> [(i, o), (o, i)]

severLink :: Graph -> Link -> Graph
severLink graph (i, o) = cutLink i o $ cutLink o i graph
  where
    cutLink :: Vertex -> Vertex -> Graph -> Graph
    cutLink input output g = updateWithKey (\_ links -> case delete input links of
                                                              [] -> Nothing
                                                              xs -> Just xs) output g

children :: Graph -> Vertex -> Maybe [Vertex]
children graph v = Data.Map.lookup v graph

-- Graph Search: BFS

initGraphSearchState :: Vertex -> GraphSearchState
initGraphSearchState v = GSS
  { mvisited   = Data.Set.fromList []
  , mfrontiers = [[v]]
  , mpaths     = []
  }

worldBfs :: World -> State GraphSearchState ()
worldBfs world = do
  frontiers <- gets mfrontiers
  case frontiers of
    []               -> return ()
    (currentPath:xs) ->
      case safeLast currentPath of
        Nothing     -> return ()
        Just vertex -> do
          when (vertex `member` (mgateways world)) $
            -- TODO: use lenses instead of this
              modify (\s -> GSS (mvisited s) (mfrontiers s) ((mpaths s) ++ [currentPath]))
          case children (mgraph world) vertex of
            Nothing -> return ()
            Just us -> do
              vis <- gets mvisited
              let newVisited = Data.Set.insert vertex vis
              let newFrontiers = nub $ filter (not . null) $
                    (xs ++) $ map (\v -> currentPath ++ [v]) $
                    filter (not . (`member` newVisited)) $ us
              modify (\s -> GSS newVisited newFrontiers (mpaths s))
              worldBfs world

runWorldBfs :: World -> GraphSearchState
runWorldBfs world = execState (worldBfs world)
                              (initGraphSearchState (magent world))

runWorldBfs2 :: World -> (Vertex -> Bool) -> GraphSearchState
runWorldBfs2 world p = execState (worldBfs2 world p)
                                 (initGraphSearchState (magent world))

-- Parsers

parseLink :: String -> Maybe Link
parseLink link =
    case (fmap reads (words link)) :: [[(Vertex, String)]] of
      [[(i, _)], [(o, _)]] -> Just $ (i, o)
      _                    -> Nothing

parseGateway :: String -> Maybe Gateway
parseGateway gateway =
    case reads gateway :: [(Gateway, String)] of
      [(g, _)] -> Just g
      _        -> Nothing

parseNetwork :: String -> Maybe (Int, Int, Int)
parseNetwork network =
    case (fmap reads (words network)) :: [[(Int, String)]] of
      [[(n, _)], [(l, _)], [(e, _)]] -> Just $ (n, l, e)
      _                              -> Nothing

parsePosition :: String -> Maybe Position
parsePosition position =
    case reads position :: [(Position, String)] of
      [(pos, _)] -> Just pos
      _          -> Nothing

parseGameLinksAndGateways :: [String] -> [String] -> Maybe (Graph, Set Gateway)
parseGameLinksAndGateways links gateways = do
    l <- traverse parseLink links
    g <- traverse parseGateway gateways
    return (makeGraph l, Data.Set.fromList g)

parseGameInput :: [String] -> Maybe (Int, Int, Int, Graph, Set Gateway)
parseGameInput (network:content) = do
  (n, l, e) <- parseNetwork network
  (graph, gateways) <- parseGameLinksAndGateways (take l content) (drop l content)
  return (n, l, e, graph, gateways)
parseGameInput _ = Nothing

parseGameLoop :: String -> Maybe Position
parseGameLoop = parsePosition


-- Second attempt

dangerousVertices :: World -> Set Vertex
dangerousVertices (World gs g _) = S.difference (S.fromList (keepDangerous gatewayChildren)) gs
  where
    gatewayChildren :: [Vertex]
    gatewayChildren = concat $ catMaybes $ fmap (children g) (S.toList gs)

    keepDangerous :: [Vertex] -> [Vertex]
    keepDangerous = catMaybes . map safeHead . filter ((>1) . length) . group . sort


worldBfs2 :: World -> (Vertex -> Bool) -> State GraphSearchState ()
worldBfs2 world p = do
  frontiers <- gets mfrontiers
  case frontiers of
    []               -> return ()
    (currentPath:xs) ->
      case safeLast currentPath of
        Nothing     -> return ()
        Just vertex -> do
          when (p vertex) $
            -- TODO: use lenses instead of this
              modify (\s -> GSS (mvisited s) (mfrontiers s) ((mpaths s) ++ [currentPath]))
          case children (mgraph world) vertex of
            Nothing -> return ()
            Just us -> do
              vis <- gets mvisited
              let newVisited = S.insert vertex vis
              let newFrontiers =
                    nub $
                    filter (not . null) $
                    (xs ++) $ map (\v -> currentPath ++ [v]) $
                    filter (not . (`member` (S.union (mgateways world) newVisited))) $
                    us
              modify (\s -> GSS newVisited newFrontiers (mpaths s))
              worldBfs2 world p

childrenInGateways :: World -> Vertex -> Bool
childrenInGateways (World gateways graph _) v =
  case children graph v of
    Nothing -> False
    Just xs -> any (`member` gateways) xs

distance :: World -> Path -> Int
distance world = length . filter (not . (childrenInGateways world))

dangerousVerticesMapping :: World -> Map Vertex ([Path], Set Gateway)
dangerousVerticesMapping world@(World gateways graph _) =
  M.mapWithKey (\k paths -> case children graph k of
                                     Nothing -> (paths, S.fromList [])
                                     Just xs -> (paths, Data.Set.intersection (Data.Set.fromList xs) gateways)) (pathsToVertex dangerSet)

  where
    dangerSet :: Set Vertex
    dangerSet = dangerousVertices world

    pathsTo :: Set Vertex -> [Path]
    pathsTo set = mpaths $ runWorldBfs2 world (`member` set)

    pathsToVertex :: Set Vertex -> Map Vertex [Path]
    pathsToVertex set = fromListWith (++) [(last path, [path]) | path <- (pathsTo set)]

pickActionFromAdjacentGateways :: World -> Maybe Action
pickActionFromAdjacentGateways (World gs g pos) = do
  cs <- children g pos
  e <- find (`member` gs) cs
  return $ Sever (pos, e)

pickActionFromDangerousVertices :: World -> Maybe Action
pickActionFromDangerousVertices world =
  case sortAssocs (M.assocs (dangerousVerticesMapping world)) of
    []               -> Nothing
    ((k, (_, gs)):_) -> Just $ Sever (k, head (S.toList gs))

  where
    sortAssocs :: [(Vertex, ([Path], Set Gateway))] -> [(Vertex, ([Path], Set Gateway))]
    sortAssocs = sortBy (comparing (uncurry heuristic . snd))

    heuristic :: [Path] -> Set Gateway -> Int
    heuristic ps gs = minimum (map (distance world) ps) - length gs

pickActionFromRemainingActions :: World -> Maybe Action
pickActionFromRemainingActions world = safeHead $ generatePlayerActions world

pickAction :: World -> Maybe Action
pickAction world@(World _ _ position)
  | childrenInGateways world position = pickActionFromAdjacentGateways world
  | otherwise = pickActionFromDangerousVertices world <|> pickActionFromRemainingActions world

-- Main Game Loop

debugGameInput :: Graph -> Set Gateway -> IO ()
debugGameInput graph gateways = do
  hPutStrLn stderr (show graph)
  hPutStrLn stderr (show gateways)

parsingFailed :: IO ()
parsingFailed = hPutStrLn stderr "Parsing failed..."

pickActionFailed :: IO ()
pickActionFailed = hPutStrLn stderr "No action found..."

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    mnetwork <- fmap parseNetwork getLine
    case mnetwork of
      Nothing -> parsingFailed >> return ()
      Just (_, l, e) -> do
        ls <- replicateM l getLine
        gs <- replicateM e getLine
        case parseGameLinksAndGateways ls gs of
          Nothing                -> parsingFailed >> return ()
          Just (graph, gateways) -> gameLoop graph gateways

gameLoop :: Graph -> Set Gateway -> IO ()
gameLoop graph gateways = do
  mposition <- fmap parseGameLoop getLine
  case mposition of
    Nothing       -> parsingFailed >> return ()
    Just position ->
      let world = (World gateways graph position)
      -- First attempt
          -- gameTree = generateGameTree Player world
      -- in case pickPlayerAction 3 gameTree of
      --      Just action@(Sever (i, o)) -> do
      --        putStrLn $ show i ++ " " ++ show o
      --        gameLoop ref (mgraph (performAction action world)) gateways
      --      _ -> hPutStrLn stderr "No action found..." >> return ()
      in case pickAction world of
           Just action@(Sever (i, o)) -> do
             putStrLn $ show i ++ " " ++ show o
             gameLoop (mgraph (performAction action world)) gateways
           _ -> pickActionFailed >> return ()
