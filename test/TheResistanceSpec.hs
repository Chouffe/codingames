module TheResistanceSpec (spec) where

import qualified Data.List     as L
import qualified Data.Set      as S
import           Test.Hspec
import           TheResistance

spec :: Spec
spec =
  describe "TheResistance" $ do
    it "should Fail" $
      1 `shouldBe` 0
