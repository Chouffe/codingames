module CGXFormatterSpec (spec) where

import           CGXFormatter
import           Control.Monad
import           Test.Hspec

puzzles :: [(Int, CGXValue)]
puzzles = [ (1, CGXPrimitive (B True))
          , (2, CGXPrimitive (S " Content with spaces and\ttabs"))
          , (3, CGXBlock [CGXPrimitive (N 0)])
          , (4, CGXBlock [CGXPrimitive (N 0), CGXPrimitive (N 1), CGXPrimitive (N 2)])
          , (5, CGXBlock [CGXBlock [CGXPrimitive (B True)]])
          , (6, CGXBlock [])
          , (7, CGXBlock [ CGXBlock [CGXPrimitive (B True)]
                         , CGXBlock [CGXPrimitive (B False)]
                         , CGXBlock [CGXPrimitive (N 0)]
                         ])
          , (8, CGXBlock [CGXKeyValue "key" (CGXPrimitive (S "value"))])
          , (9, CGXBlock [ CGXKeyValue "k1" (CGXPrimitive (S "v1"))
                         , CGXKeyValue "k2" (CGXPrimitive (S "v2"))
                         , CGXPrimitive (N 123)
                         , CGXKeyValue "k3" (CGXPrimitive (S "v3"))
                         ])
          , (10, (CGXKeyValue "users"
          (CGXBlock [CGXBlock [CGXKeyValue "id" (CGXPrimitive (N 10)),CGXKeyValue "name" (CGXPrimitive (S "Serge")),CGXKeyValue "roles" (CGXBlock [CGXPrimitive (S "visitor"),CGXPrimitive (S "moderator")])],CGXBlock [CGXKeyValue "id" (CGXPrimitive (N 11)),CGXKeyValue "name" (CGXPrimitive (S "Biales"))],CGXPrimitive (B True)])))
          , (11, (CGXBlock [CGXKeyValue "menu" (CGXBlock [CGXKeyValue "id" (CGXPrimitive (S "file")),CGXKeyValue "value" (CGXPrimitive (S "File")),CGXKeyValue "popup" (CGXBlock [CGXKeyValue "menuitem" (CGXBlock [CGXBlock [CGXKeyValue "value" (CGXPrimitive (S "New")),CGXKeyValue "onclick" (CGXPrimitive (S "CreateNewDoc()"))],CGXBlock [CGXKeyValue "value" (CGXPrimitive (S "Open")),CGXKeyValue "onclick" (CGXPrimitive (S "OpenDoc()"))],CGXBlock [CGXKeyValue "value" (CGXPrimitive (S "Close")),CGXKeyValue "onclick" (CGXPrimitive (S "CloseDoc()"))]])]),CGXBlock []])]))
          , (12, (CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXBlock [CGXPrimitive (N 0)]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]))
          ]

specPath :: Int -> FilePath
specPath i = "test/input/cgxformatter/spec_" ++ show i ++ ".txt"

specPathOutput :: Int -> FilePath
specPathOutput i = "test/output/cgxformatter/spec_" ++ show i ++ ".txt"


withSpecInput :: Int -> String -> (String -> Expectation) -> Spec
withSpecInput i title handler =
  describe ("spec " ++ show i) $ do
    it title $ do
      content <- readFile (specPath i)
      handler content

specInputOutput :: Int -> Spec
specInputOutput i =
  describe ("spec " ++ show i) $ do
    it "pretty prints properly" $ do
      input <- readFile (specPath i)
      output <- readFile (specPathOutput i)
      case readCGX' input of
        Left e    -> fail $ show e
        Right cgx -> show (pprint cgx) ++ "\n" `shouldBe` output


spec :: Spec
spec =
  describe "CGXFormatter" $ do
    forM_ puzzles (\(i, expected) -> do
      withSpecInput i "parses properly"
        (\input -> readCGX' input `shouldBe` Right expected))

    forM_ [1..12] specInputOutput
