module Main where

import           Criterion.Main
import qualified SkynetRevolution2Spec
import           Test.Hspec
import qualified VoxCodeiSpec
import qualified WarSpec
import qualified SuperComputerSpec

main :: IO ()
main = do
  -- defaultMain [VoxCodeiSpec.benchmark]
  -- hspec VoxCodeiSpec.spec
  hspec SuperComputerSpec.spec
  -- WarSpec.spec
  -- SkynetRevolution2Spec.spec
