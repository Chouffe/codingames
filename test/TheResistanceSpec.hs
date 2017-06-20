module TheResistanceSpec (spec) where

import           Control.Monad
import qualified Data.List     as L
import qualified Data.Set      as S
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
          show (messageNumber m (dictionary morseTable ws))
            `shouldBe` output

spec :: Spec
spec = describe "TheResistance" $
  forM_ [1..3] specInputOutput  -- TODO: memoize for number 4
