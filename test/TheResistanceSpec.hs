module TheResistanceSpec (spec) where

import           Control.Monad
import           Test.Hspec
import           TheResistance

specPathInput :: Int -> FilePath
specPathInput i = "test/input/theresistance/spec_" ++ show i ++ ".txt"

specPathOutput :: Int -> FilePath
specPathOutput i = "test/output/theresistance/spec_" ++ show i ++ ".txt"

specInputOutput :: Int -> Spec
specInputOutput i =
  describe ("spec " ++ show i) $
    it "finds the proper number of messages" $ do
      input <- readFile (specPathInput i)
      output <- head . lines <$> readFile (specPathOutput i)
      case parseGameInput input of
        Nothing      -> fail "Could not parse input/output spec files"
        Just (m, ws) ->
          show (runMemoizedMessageNumber (buildDict morseTable ws) m)
            `shouldBe` output

spec :: Spec
spec = describe "TheResistance" $ do
  describe "Provided Tests" $
    forM_ [1..4] specInputOutput

  describe "Extra tests" $
    forM_ [5..7] specInputOutput

  describe "Multiple Words" $
    forM_ [8..9] specInputOutput
