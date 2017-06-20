module Main where

import qualified CGXFormatterSpec
import qualified SkynetRevolution2Spec
import qualified SuperComputerSpec
import qualified TheResistanceSpec
import qualified VoxCodeiSpec
import qualified WarSpec

import           Criterion.Main
import           Test.Hspec

main :: IO ()
main = do
  -- hspec CGXFormatterSpec.spec
  hspec TheResistanceSpec.spec
  -- defaultMain [VoxCodeiSpec.benchmark]
  -- hspec VoxCodeiSpec.spec
  -- WarSpec.spec
  -- SkynetRevolution2Spec.spec
