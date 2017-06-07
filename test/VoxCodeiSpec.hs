{-# LANGUAGE ScopedTypeVariables #-}

module VoxCodeiSpec (spec) where

import qualified Data.List       as L
import           Test.Hspec
import           Test.QuickCheck
import           VoxCodei

specPath :: Int -> FilePath
specPath i = "test/input/voxcodei/spec_" ++ show i ++ ".txt"

getFirewall :: Int -> IO (Maybe Firewall)
getFirewall i = parseFirewall . lines <$> readFile (specPath i)

parsingSpec :: Int -> Spec
parsingSpec i =
  describe ("Parsing Spec" ++ show i) $ do
    it "show . parseFirewall . line == id" $ do
      content <- readFile $ specPath i
      show <$> parseFirewall (lines content) `shouldBe` Just content


spec :: Spec
spec = describe "VoxCodei" $ do
  describe "Parsing" $ do
    mapM_ parsingSpec [1..11]

  describe "shiftPosition" $ do
    describe "UP" $ do
      it "shifts up" $ do
        mfirewall <- getFirewall 1
        case mfirewall of
          Nothing -> fail "could not parse spec 1"
          Just firewall -> shiftPosition firewall U (Position (0, 1)) `shouldBe` Just (Position (0, 0))

      it "fails to shift Up" $ do
        mfirewall <- getFirewall 1
        case mfirewall of
          Nothing -> fail "could not parse spec 1"
          Just firewall -> shiftPosition firewall U (Position (0, 0)) `shouldBe` Nothing

    describe "DOWN" $ do
      it "shifts down" $ do
        mfirewall <- getFirewall 1
        case mfirewall of
          Nothing -> fail "could not parse spec 1"
          Just firewall -> shiftPosition firewall D (Position (0, 0)) `shouldBe` Just (Position (0, 1))

      it "fails to shift Down" $ do
        mfirewall <- getFirewall 1
        case mfirewall of
          Nothing -> fail "could not parse spec 1"
          Just firewall -> shiftPosition firewall D (Position (0, 2)) `shouldBe` Nothing

    describe "LEFT" $ do
      it "shifts left" $ do
        mfirewall <- getFirewall 1
        case mfirewall of
          Nothing -> fail "could not parse spec 1"
          Just firewall -> shiftPosition firewall L (Position (1, 0)) `shouldBe` Just (Position (0, 0))

      it "fails to shift Left" $ do
        mfirewall <- getFirewall 1
        case mfirewall of
          Nothing -> fail "could not parse spec 1"
          Just firewall -> shiftPosition firewall L (Position (0, 0)) `shouldBe` Nothing

    describe "RIGHT" $ do
      it "shifts right" $ do
        mfirewall <- getFirewall 1
        case mfirewall of
          Nothing -> fail "could not parse spec 1"
          Just firewall -> shiftPosition firewall R (Position (2, 0)) `shouldBe` Just (Position (3, 0))

      it "fails to shift Right" $ do
        mfirewall <- getFirewall 1
        case mfirewall of
          Nothing -> fail "could not parse spec 1"
          Just firewall -> shiftPosition firewall R (Position (3, 0)) `shouldBe` Nothing

  describe "deleteSurveillanceNodeAt" $ do
    it "deletes the surveillance node" $ do
      mfirewall <- getFirewall 1
      case mfirewall of
        Nothing -> fail "could not parse spec 1"
        Just firewall -> L.find (== S) (_cells firewall') `shouldBe` Nothing
                         where firewall' = deleteSurveillanceNodeAt (Position (1, 1)) firewall

    it "does not delete the surveillance node" $ do
      mfirewall <- getFirewall 1
      case mfirewall of
        Nothing -> fail "could not parse spec 1"
        Just firewall -> L.find (== S) (_cells firewall') `shouldBe` Just S
                         where firewall' = deleteSurveillanceNodeAt (Position (0, 0)) firewall

  -- TODO
  describe "explode" $ do
    describe "node type" $ do
      it "does explode a surveillance node" $ pending
      it "does not exploed indestructible nodes" $ pending

    describe "Range of the explosion" $ do
      it "propagates 3 nodes Up" $ pending
      it "propagates 3 nodes Down" $ pending
      it "propagates 3 nodes Left" $ pending
      it "propagates 3 nodes Right" $ pending

    describe "Recurive explosion" $ do
      it "explodes another bomb" $ pending
      it "does not explode bombs that are too far away" $ pending

  -- TODO
  describe "tick" $ do
    it "decrements remainingTurns" $ pending
    it "removes bombs that have a ttl <= 0" $ pending
    it "decreases ttl for bombs" $ pending
    it "explodes all bombs with a ttl <= 0" $ pending
