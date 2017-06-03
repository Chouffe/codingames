{-# LANGUAGE ScopedTypeVariables #-}
module SkynetRevolution2Spec where

import           Data.List         (intersperse)
import qualified Data.Set          as S
import           SkynetRevolution2 (Link, Turn (..), Vertex, makeGraph,
                                    nextTurn, parseGameInput, parseGateway,
                                    parseLink, parseNetwork, safeHead, safeLast)
import           Test.Hspec
import           Test.QuickCheck

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

      describe "parseGameInput" $ do
        it "builds the correct graph" $ property $
          \(links :: [Link]) -> parseGameInput (map (\(i, o) -> show i ++ " " ++ show o) links) [] == Just (makeGraph links, S.empty)
        it "builds the correct gateways" $ property $
          \(vs :: [Vertex]) -> parseGameInput [] (map show vs) == Just (makeGraph [], S.fromList vs)
        it "builds correct graph and gateways" $ property $
          \(links :: [Link]) (vs :: [Vertex]) -> parseGameInput (map (\(i, o) -> show i ++ " " ++ show o) links) (map show vs) == Just (makeGraph links, S.fromList vs)

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
