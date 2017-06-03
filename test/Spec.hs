module Main where

import           SkynetRevolution2Spec (spec)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  SkynetRevolution2Spec.spec
