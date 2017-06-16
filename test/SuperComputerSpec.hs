module SuperComputerSpec (spec) where

import           Control.Monad
import           Test.Hspec
import           SuperComputer

puzzles :: [(Int, Int)]
puzzles = [(1, 3), (2, 4)]

specPath :: Int -> FilePath
specPath i = "test/input/supercomputer/spec_" ++ show i ++ ".txt"

withSpecActivities :: Int -> String -> ([Activity]-> Expectation) -> Spec
withSpecActivities i title handler =
  describe ("spec " ++ show i) $ do
    it title $ do
      mactivities <- parseGameInput . lines <$> readFile (specPath i)
      maybe (fail ("could not parse spec " ++ show i)) handler mactivities

spec :: Spec
spec =
  describe "SuperComputer" $ do
    forM_ puzzles (\(i, expected) -> do
      withSpecActivities i "should solve puzzle"
        ((`shouldBe` expected) . length . activitySelector))
