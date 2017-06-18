module CGXFormatterSpec (spec) where

import           CGXFormatter
import qualified Data.List       as L
import qualified Data.Set        as S
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  describe "CGXFormatter" $ do
    it "fails" $ 1 `shouldBe` 0
