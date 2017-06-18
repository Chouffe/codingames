module Main where

import qualified CGXFormatterSpec
import           Criterion.Main
import qualified SkynetRevolution2Spec
import           Test.Hspec
import qualified VoxCodeiSpec
import qualified WarSpec

main :: IO ()
main = do
  hspec CGXFormatterSpec.spec
  -- defaultMain [VoxCodeiSpec.benchmark]
  -- hspec VoxCodeiSpec.spec
  -- WarSpec.spec
  -- SkynetRevolution2Spec.spec
