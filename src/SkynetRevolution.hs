module SkynetRevolution where

import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.List                        (delete, groupBy, nub, sortBy)
import           Data.Map                         (Map, empty, insert, lookup,
                                                   updateWithKey)
import           Data.Set                         (Set, fromList, insert,
                                                   member)
import           System.IO

-- Data Modeling

type Vertex = Int
type Gateway = Vertex
type Position = Vertex
type Link = (Vertex, Vertex)
type Graph = Map Vertex [Vertex]

-- Graph API

initGraph :: Graph
initGraph = empty

makeGraph :: [Link] -> Graph
makeGraph links =
    foldr (\ls graph -> case ls of
                          []         -> graph
                          ((i, _):_) -> Data.Map.insert i (fmap snd ls) graph
          ) initGraph $
            groupBy (\(a, _) (b, _) -> a == b) $
            sortBy (\(a, _) (b, _) -> compare a b) $
            nub $
            links >>= \(i, o) -> [(i, o), (o, i)]

cutLink :: Graph -> Link -> Graph
cutLink graph (i, o) =
    updateWithKey (\_ links -> case delete o links of
                                 [] -> Nothing
                                 xs -> Just xs
                                 ) i $
    updateWithKey (\_ links -> case delete i links of
                                 [] -> Nothing
                                 xs -> Just xs
                                 ) o $
    graph

-- Game Logic

children :: Graph -> Vertex -> Maybe [Vertex]
children graph v = Data.Map.lookup v graph

data GraphState = GSt
  { visited   :: Set Vertex
  , frontier  :: [Vertex]
  , linkToCut :: Maybe Link
  } deriving (Show)

initGraphState :: Vertex -> GraphState
initGraphState v = GSt { visited = fromList []
                      , frontier = [v]
                      , linkToCut = Nothing
                      }

bfs :: Graph -> Set Gateway -> State GraphState ()
bfs graph gateways = do
  gst <- get
  case frontier gst of
    []     -> return ()
    (v:vs) ->
      case children graph v of
          Nothing -> bfs graph gateways
          Just us ->
            let newVisited = Data.Set.insert v (visited gst)
                newFrontier = nub (vs ++ us)
            in case filter (\e -> member e gateways) us of
                 [] -> do
                     put (GSt newVisited newFrontier Nothing)
                     bfs graph gateways
                 (o:_) -> put (GSt newVisited newFrontier (Just (v, o)))

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

parseGameInput :: [String] -> [String] -> Maybe (Graph, Set Gateway)
parseGameInput links gateways = do
    l <- traverse parseLink links
    g <- traverse parseGateway gateways
    return (makeGraph l, fromList g)

parseGameLoop :: String -> Maybe Position
parseGameLoop = parsePosition

debugGameInput :: Graph -> Set Gateway -> IO ()
debugGameInput graph gateways = do
  hPutStrLn stderr (show graph)
  hPutStrLn stderr (show gateways)

debugBfs :: GraphState -> IO ()
debugBfs gst = hPutStrLn stderr (show gst)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    mnetwork <- fmap parseNetwork getLine
    case mnetwork of
      Nothing -> return ()                     -- Parsing failed
      Just (_, l, e) -> do
        ls <- replicateM l getLine
        gs <- replicateM e getLine
        case parseGameInput ls gs of
          Nothing                -> return ()  -- Parsing failed
          Just (graph, gateways) -> gameLoop graph gateways

gameLoop :: Graph -> Set Gateway -> IO ()
gameLoop graph gateways = do
  mposition <- fmap parseGameLoop getLine
  case mposition of
    Nothing       -> return ()                 -- Parsing failed
    Just position ->
      let gst = execState (bfs graph gateways)
                          (initGraphState position)
      in do
        debugBfs gst
        case linkToCut gst of
             Nothing -> return ()  -- Agent cannot reach any gateways!
             Just link@(i, o) -> do
               putStrLn $ show i ++ " " ++ show o
               gameLoop (cutLink graph link) gateways
