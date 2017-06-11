module Main where

import           Criterion.Main
import qualified SkynetRevolution2Spec
import           Test.Hspec
import qualified VoxCodeiSpec
import qualified WarSpec

main :: IO ()
main = do
  defaultMain [VoxCodeiSpec.benchmark]
  hspec VoxCodeiSpec.spec
  -- WarSpec.spec
  -- SkynetRevolution2Spec.spec
