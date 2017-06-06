{-# LANGUAGE ScopedTypeVariables #-}

module VoxCodeiSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           VoxCodei

specPath :: Int -> FilePath
specPath i = "test/input/voxcodei/spec_" ++ show i ++ ".txt"

parsingSpec :: Int -> Spec
parsingSpec i =
  describe ("Parsing Spec" ++ show i) $ do
    it "show . parseFirewall . line == id" $ do
      content <- readFile $ specPath i
      fmap show (parseFirewall (lines content)) `shouldBe` (Just content)


spec :: Spec
spec = describe "VoxCodei" $ do
  describe "Parsing" $ do
    mapM_ parsingSpec [1..11]
