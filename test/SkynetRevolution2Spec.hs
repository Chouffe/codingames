{-# LANGUAGE ScopedTypeVariables #-}

module SkynetRevolution2Spec (spec) where

import           Data.List         (intersperse)
import qualified Data.Map          as M
import qualified Data.Set          as S
import           SkynetRevolution2 (Gateway, Graph, Link, Position, Turn (..),
                                    Vertex, World (..), dangerousVertices,
                                    makeGraph, nextTurn, parseGameInput,
                                    parseGameLinksAndGateways, parseGateway,
                                    parseLink, parseNetwork, safeHead, safeLast)
import           Test.Hspec
import           Test.QuickCheck

specPath :: Int -> FilePath
specPath i = "test/input/skynetrevolution2/spec_" ++ show i ++ ".txt"

getGameInput :: Int -> IO (Maybe (Int, Int, Int, Graph, S.Set Gateway))
getGameInput i = parseGameInput . lines <$> readFile (specPath i)

getWorld :: Int -> Position -> IO (Maybe World)
getWorld i pos = do
  m <- getGameInput i
  case m of
    Nothing                         -> return Nothing
    Just (_, _, _, graph, gateways) -> return $ Just $ World gateways graph pos

parsingSpec :: Int -> Spec
parsingSpec i =
  describe ("Parsing Spec: " ++ show i) $ do
    it "contains the proper number of gateways" $ do
      gameInput <- getGameInput i
      case gameInput of
        Nothing                     -> fail "could not parse"
        Just (_, _, e, _, gateways) -> e `shouldBe` (length gateways)

    it "contains the proper number of nodes" $ do
      gameInput <- getGameInput i
      case gameInput of
        Nothing                  -> fail "could not parse"
        Just (n, _, _, graph, _) -> n `shouldBe` (length (M.keys graph))

    it "contains the proper number of links" $ do
      gameInput <- getGameInput i
      case gameInput of
        Nothing                  -> fail "could not parse"
        Just (_, l, _, graph, _) -> 2 * l `shouldBe` (length (concat (M.elems graph)))

dangerousVertexSpec :: Int -> Position -> [Vertex] -> Spec
dangerousVertexSpec i position expected =
  describe ("spec " ++ show i) $
    it "contains the correct dangerous nodes" $ do
      mworld <- getWorld i position
      case mworld of
        Nothing    -> fail "could not parse"
        Just world -> dangerousVertices world `shouldBe` (S.fromList expected)

spec :: Spec
spec =
  describe "SkynetRevolution2" $ do

    describe "utils" $ do
      describe "safeHead" $ do
        it "returns Nothing on empty list" $ do
          (safeHead []) `shouldBe` (Nothing :: Maybe Int)
        it "returns head of non empty list" $
          property $ \(xs :: [Int]) -> not (null xs) ==> safeHead xs == Just (head xs)

      describe "safeLast" $ do
        it "returns Nothing on empty list" $ do
          (safeLast [] :: Maybe Int) `shouldBe` (Nothing :: Maybe Int)
        it "returns tail of non empty list" $
          property $ \(xs :: [Int]) -> not (null xs) ==> safeLast xs == Just (last xs)

    describe "Parsers" $ do
      describe  "parseLink" $ do
        it "returns a valid link" $
          property $ \i o -> parseLink (show i ++ " " ++ show o) == Just ((i, o) :: (Int, Int))
        it "fails parsing for char on first position" $
          property $ \(i :: Int) (c :: Char) -> parseLink (show c ++ " " ++ show i) == Nothing
        it "fails parsing for char on second position" $
          property $ \(i :: Int) (c :: Char) -> parseLink (show i ++ " " ++ show c) == Nothing
      describe "parseGateway" $ do
        it "returns a valid gateway" $
          property $ \(g :: Int) -> parseGateway (show g) == Just g
        it "fails parsing" $
          property $ \(c :: Char) -> parseGateway (show c) == Nothing

      describe "parseNetwork" $ do
        it "returns a triple representing the network" $
          property $ \(n :: Int) (l :: Int) (e :: Int) -> parseNetwork (concat (intersperse " " (fmap show [n, l, e]))) == Just (n, l, e)

      describe "parseGameInput" $ mapM_ parsingSpec [1..6]


      describe "parseGameLinksAndGateways" $ do
        it "builds the correct graph" $ property $
          \(links :: [Link]) -> parseGameLinksAndGateways (map (\(i, o) -> show i ++ " " ++ show o) links) [] == Just (makeGraph links, S.empty)
        it "builds the correct gateways" $ property $
          \(vs :: [Vertex]) -> parseGameLinksAndGateways [] (map show vs) == Just (makeGraph [], S.fromList vs)
        it "builds correct graph and gateways" $ property $
          \(links :: [Link]) (vs :: [Vertex]) -> parseGameLinksAndGateways (map (\(i, o) -> show i ++ " " ++ show o) links) (map show vs) == Just (makeGraph links, S.fromList vs)

    describe "nextTurn" $ do
      it "returns Player for Agent" $
        (nextTurn Player) `shouldBe` Agent
      it "returns Agent for Player" $
        (nextTurn Agent) `shouldBe` Player

    describe "generateGameTree" $ do
      it "generates proper world for player" $ pending
      it "generates proper world for agent" $ pending

    describe "playerWon" $ do
      it "is not won" pending
      it "is won" pending

    describe "playerLost" $ do
      it "is not lost" pending
      it "is lost" pending

    describe "runWorldBfs" $ do
      it "contains the correct paths" pending

    describe "generateAgentActions" $ do
      it "generates the correct agent actions" pending

    describe "generatePlayerActions" $ do
      it "generates the correct player actions" pending

    describe "remainingSeparableLinks" $ do
      it "finds all separable links" pending

    describe "dangerousVertices" $
      mapM_ (\(i, position, expected) -> dangerousVertexSpec i position expected) $
        [ (1, 0, [3])
        , (2, 0, [1, 2])
        , (3, 0, [6])
        , (4, 0, [1, 6, 7])
        , (5, 0, [17, 27])
        , (6, 0, [3, 20, 5, 33, 40, 47, 27])
        ]