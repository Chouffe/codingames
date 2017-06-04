{-# LANGUAGE ScopedTypeVariables #-}

module WarSpec where

import           Test.Hspec
import           War

gameSpec :: Int -> Spec
gameSpec i =
  describe ("Game Spec: " ++ show i) $
    it "should pass" $ do
      testInput <- readFile $ "test/input/war/warspec_" ++ show i ++ ".txt"
      testOutput <- readFile $ "test/output/war/warspec_" ++ show i ++ ".txt"
      case parseDecks (lines testInput) of
        Nothing             -> return ()
        Just (d1, d2) ->
          case parseGameResult testOutput of
            Nothing         -> return ()
            Just gameResult -> (fight d1 d2) `shouldBe` Just gameResult


spec :: Spec
spec = describe "Game Specs" $ mapM_ gameSpec [1..9]
