{-# LANGUAGE ScopedTypeVariables #-}

module VoxCodeiSpec (spec) where

import           Control.Lens
import qualified Data.List    as L
import qualified Data.Set     as S
import           Test.Hspec
-- import           Test.QuickCheck
import           VoxCodei

specPath :: Int -> FilePath
specPath i = "test/input/voxcodei/spec_" ++ show i ++ ".txt"

updateNodeWith :: Cell -> Cell -> Firewall -> Firewall
updateNodeWith c c' = over cells (fmap (\e -> if e == c then c' else e))

removeSurveillanceNodes :: Firewall -> Firewall
removeSurveillanceNodes = updateNodeWith S E

replaceWithIndestructibleNodes :: Firewall -> Firewall
replaceWithIndestructibleNodes = updateNodeWith S I

getFirewall :: Int -> IO (Maybe Firewall)
getFirewall i = parseFirewall . lines <$> readFile (specPath i)

parsingSpec :: Int -> Spec
parsingSpec i =
  describe ("Parsing Spec" ++ show i) $ do
    it "show . parseFirewall . line == id" $ do
      content <- readFile $ specPath i
      show <$> parseFirewall (lines content) `shouldBe` Just content

withSpecFirewall :: Int -> String -> (Firewall -> Expectation) -> Spec
withSpecFirewall i title handler =
  describe ("spec " ++ show i) $ do
    it title $ do
      mfirewall <- getFirewall i
      maybe (fail ("could not parse spec " ++ show i)) handler mfirewall

withGameState :: Int -> String -> Int -> Int -> (GameState -> Expectation) -> Spec
withGameState i title rturns rbombs handler =
  withSpecFirewall i title
    (\f -> handler (initGameState f rturns rbombs))

spec :: Spec
spec = describe "VoxCodei" $ do
  describe "Parsing" $ do
    mapM_ parsingSpec [1..11]

  describe "shiftPosition" $ do
    describe "UP" $ do
      withSpecFirewall 1 "shifts up"
        (\f -> shiftPosition f U (position 0 1) `shouldBe` Just (position 0 0))

      withSpecFirewall 1 "fails to shift up"
        (\f -> shiftPosition f U (position 0 0) `shouldBe` Nothing)

    describe "DOWN" $ do
      withSpecFirewall 1 "shifts down"
        (\f -> shiftPosition f D (position 0 0) `shouldBe` Just (position 0 1))

      withSpecFirewall 1 "fails to shift down"
        (\f -> shiftPosition f D (position 0 2) `shouldBe` Nothing)

    describe "LEFT" $ do
      withSpecFirewall 1 "shifts left"
        (\f -> shiftPosition f L (position 1 0) `shouldBe` Just (position 0 0))

      withSpecFirewall 1 "fails to shift left"
        (\f -> shiftPosition f L (position 0 0) `shouldBe` Nothing)

    describe "RIGHT" $ do
      withSpecFirewall 1 "shifts right"
        (\f -> shiftPosition f R (position 2 0) `shouldBe` Just (position 3 0))

      withSpecFirewall 1 "fails to shift right"
        (\f -> shiftPosition f R (position 3 0) `shouldBe` Nothing)

  describe "deleteSurveillanceNodeAt" $ do
    withSpecFirewall 1 "deletes the surveillance node"
      (\f -> let f' = deleteSurveillanceNodeAt (position 1 1) f
             in L.find (== S) (_cells f') `shouldBe` Nothing)

    withSpecFirewall 1 "does not delete the surveillance node"
      (\f -> let f' = deleteSurveillanceNodeAt (position 0 0) f
             in L.find (== S) (_cells f') `shouldBe` Just S)

  describe "explode" $ do
    describe "node type" $ do
      describe "Surveillance Nodes" $ do
        withSpecFirewall 1 "it does explode a surveillance node"
          (\f -> explode S.empty (range 3) (bomb (position 0 1) (ttl 0)) f
                   `shouldBe` (removeSurveillanceNodes f))

        withSpecFirewall 1 "it does not explode a surveillance node - ttl = 0"
          (\f -> explode S.empty (range 3) (bomb (position 0 0) (ttl 0)) f `shouldBe` f)

        withSpecFirewall 1 "it does not explode a surveillance node because ttl > 0"
          (\f -> explode S.empty (range 3) (bomb (position 0 1) (ttl 1)) f `shouldBe` f)

      describe "Indestructible Nodes" $ do
        withSpecFirewall 1 "it does not explode an indestructible node from UP"
          (\f -> let f' = replaceWithIndestructibleNodes f
                 in explode S.empty (range 3) (bomb (position 1 0) (ttl 0)) f' `shouldBe` f')

        withSpecFirewall 1 "it does not explode an indestructible node from LEFT"
          (\f -> let f' = replaceWithIndestructibleNodes f
                 in explode S.empty (range 3) (bomb (position 0 1) (ttl 0)) f' `shouldBe` f')

        withSpecFirewall 1 "it does not explode an indestructible node from DOWN"
          (\f -> let f' = replaceWithIndestructibleNodes f
                 in explode S.empty (range 3) (bomb (position 1 2) (ttl 0)) f' `shouldBe` f')

        withSpecFirewall 1 "it does not explode an indestructible node from RIGHT"
          (\f -> let f' = replaceWithIndestructibleNodes f
                 in explode S.empty (range 3) (bomb (position 2 1) (ttl 0)) f' `shouldBe` f')

    describe "Range of the explosion" $ do

      let f = makeFirewall [ E, E, E, E, E, E, E
                           , E, E, E, E, E, E, E
                           , E, E, E, E, E, E, E
                           , E, E, E, S, E, E, E
                           , E, E, E, E, E, E, E
                           , E, E, E, E, E, E, E
                           , E, E, E, E, E, E, E
                           ] 7 7

      it "propagates 3 nodes Up" $
        explode S.empty (range 3) (bomb (position 3 6) (ttl 0)) f
          `shouldBe` (removeSurveillanceNodes f)

      it "propagates 3 nodes Down" $
        explode S.empty (range 3) (bomb (position 3 0) (ttl 0)) f
          `shouldBe` (removeSurveillanceNodes f)

      it "propagates 3 nodes Left" $
        explode S.empty (range 3) (bomb (position 6 3) (ttl 0)) f
          `shouldBe` (removeSurveillanceNodes f)

      it "propagates 3 nodes Right" $
        explode S.empty (range 3) (bomb (position 0 3) (ttl 0)) f
          `shouldBe` (removeSurveillanceNodes f)

    withSpecFirewall 4 "propagates in all directions" $ do
      (\f -> explode S.empty (range 3) (bomb (position 1 1) (ttl 0)) f
        `shouldBe` (removeSurveillanceNodes f))

    describe "Recursive explosions" $ do
      withSpecFirewall 1 "explodes another bomb from RIGHT side"
        (\f -> explode (S.fromList [(bomb (position 0 1) (ttl 3))])
                       (range 3)
                       (bomb (position 0 3) (ttl 0))
                       f
          `shouldBe` (removeSurveillanceNodes f))

      withSpecFirewall 1 "explodes another bomb from LEFT side"
        (\f -> explode (S.fromList [(bomb (position 0 1) (ttl 3))])
                       (range 3)
                       (bomb (position 0 0) (ttl 0))
                       f
          `shouldBe` (removeSurveillanceNodes f))

      withSpecFirewall 1 "explodes another bomb from UP side"
        (\f -> explode (S.fromList [(bomb (position 0 1) (ttl 3))])
                       (range 3)
                       (bomb (position 0 0) (ttl 0))
                       f
          `shouldBe` (removeSurveillanceNodes f))

      withSpecFirewall 1 "explodes another bomb from DOWN side"
        (\f -> explode (S.fromList [(bomb (position 0 1) (ttl 3))])
                       (range 3)
                       (bomb (position 0 2) (ttl 0))
                       f
          `shouldBe` (removeSurveillanceNodes f))

      withSpecFirewall 1 "explosion chain"
        (\f -> explode (S.fromList [ (bomb (position 3 0) (ttl 3))
                                   , (bomb (position 3 2) (ttl 3))
                                   , (bomb (position 2 2) (ttl 3))
                                   , (bomb (position 2 1) (ttl 3))
                                   ])
                       (range 3)
                       (bomb (position 0 0) (ttl 0))
                       f
          `shouldBe` (removeSurveillanceNodes f))

      describe "far away bombs" $ do

        let f = makeFirewall [ E, E, E, E, E, E, E, E, E
                             , E, E, E, E, E, E, E, E, E
                             , E, E, E, E, E, E, E, E, E
                             , E, E, E, E, E, E, E, E, E
                             , E, E, E, E, S, E, E, E, E
                             , E, E, E, E, E, E, E, E, E
                             , E, E, E, E, E, E, E, E, E
                             , E, E, E, E, E, E, E, E, E
                             , E, E, E, E, E, E, E, E, E
                             ] 9 9

        it "does not explode surveillance node UP" $
          explode S.empty (range 3) (bomb (position 4 8) (ttl 0)) f
            `shouldBe` f

        it "does not explode surveillance node DOWN" $
          explode S.empty (range 3) (bomb (position 4 0) (ttl 0)) f
            `shouldBe` f

        it "does not explode surveillance node LEFT" $
          explode S.empty (range 3) (bomb (position 8 4) (ttl 0)) f
            `shouldBe` f

        it "does not explode surveillance node RIGHT" $
          explode S.empty (range 3) (bomb (position 0 4) (ttl 0)) f
            `shouldBe` f

  describe "tick" $ do
    withGameState 1 "decrements remainingTurns" 1 10
      (\gst -> (_remainingTurns (tick gst)) `shouldBe` 0)

    withGameState 1 "removes one bomb that has a ttl <= 0" 1 10
      (\gst -> let b = bomb (position 0 0) (ttl 0)
                   gst' = addBomb b gst
                   gst'' = tick gst
               in do
                 (_bombs gst) `shouldBe` S.empty
                 (_bombs gst') `shouldBe` S.fromList [b]
                 (_bombs gst'') `shouldBe` S.empty)

    withGameState 1 "removes all bomb that have a ttl <= 0" 1 10
      (\gst -> let bs = [ bomb (position 0 0) (ttl 0)
                        , bomb (position 2 2) (ttl 0)
                        ]
                   gst' = foldl (flip addBomb) gst bs
                   gst'' = tick gst
               in do
                 (_bombs gst) `shouldBe` S.empty
                 (_bombs gst') `shouldBe` S.fromList bs
                 (_bombs gst'') `shouldBe` S.empty)

    withGameState 1 "does not remove a bomb that has a ttl > 0" 1 10
      (\gst -> let b = bomb (position 0 0) (ttl 3)
                   gst' = addBomb b gst
                   gst'' = tick gst'
               in do
                 (_bombs gst) `shouldBe` S.empty
                 (_bombs gst') `shouldBe` S.fromList [b]
                 (_bombs gst'') `shouldBe` S.fromList [decreaseTTL b])

    withGameState 1 "decreases ttl for bombs" 1 10
      (\gst -> let bs = [ bomb (position 0 0) (ttl 3)
                        , bomb (position 1 0) (ttl 3)
                        , bomb (position 2 0) (ttl 2)
                        , bomb (position 2 1) (ttl 1)
                        ]
                   gst' = foldl (flip addBomb) gst bs
                   gst'' = tick gst'
               in do
                 (_bombs gst) `shouldBe` S.empty
                 (_bombs gst') `shouldBe` S.fromList bs
                 (_bombs gst'') `shouldBe` S.fromList (map decreaseTTL bs))


    withGameState 1 "explodes all bombs with a ttl <= 0" 1 10
      (\gst -> let bs = [ bomb (position 0 0) (ttl 0)
                        , bomb (position 2 1) (ttl 0)
                        ]
                   gst' = foldl (flip addBomb) gst bs
                   gst'' = tick gst'
               in do
                 (_bombs gst) `shouldBe` S.empty
                 (_bombs gst') `shouldBe` S.fromList bs
                 (_bombs gst'') `shouldBe` S.empty)

  -- TODO
  describe "won" $ do

    it "returns True when there are no more surveillance nodes and some remaining gameturns" pending
    it "returns False when there are no remaining game turns" pending


  -- TODO
  describe "lost" $ do

    it "returns True when there are no more game turns" pending
    it "returns True when there are no more bombs to explode and still some surveillance nodes" pending

  -- TODO
  describe "gameTree" $ do
    it "returns the correct gameTree" pending
