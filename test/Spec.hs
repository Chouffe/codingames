module Main where

import qualified SkynetRevolution2Spec
import           Test.Hspec
import qualified WarSpec

main :: IO ()
main = hspec $ do
  WarSpec.spec
  -- SkynetRevolution2Spec.spec
