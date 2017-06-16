module SuperComputer where

import           Control.Monad
import qualified Data.List           as L
import           System.IO

data Activity = Activity {start :: Int, finish :: Int}
    deriving (Eq, Show)

activitySelectorHelper :: [Activity] -> [Activity] -> [Activity]
activitySelectorHelper ys []     = ys
activitySelectorHelper [] (x:xs) = activitySelectorHelper [x] xs
activitySelectorHelper ys@((Activity _ f'):_) (a@(Activity s _):xs) =
    case s > f' of
      True  -> activitySelectorHelper (a:ys) xs
      False -> activitySelectorHelper ys xs

activitySelector :: [Activity] -> [Activity]
activitySelector = activitySelectorHelper [] . L.sortOn finish

-- Parsers

parseInt :: String -> Maybe Int
parseInt str =
  case reads str :: [(Int, String)] of
    [(n, _)] -> Just n
    _        -> Nothing

parseActivity :: String -> Maybe Activity
parseActivity str = case words str of
                      [s, d] -> do
                          s' <- parseInt s
                          d' <- parseInt d
                          return $ Activity s' (s' + d' - 1)
                      _      -> Nothing

parseGameInput :: [String] -> Maybe [Activity]
parseGameInput = traverse parseActivity

-- Game Logic

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    optn <- parseInt <$> getLine
    case optn of
      Nothing -> fail "Could not parse n"
      Just n -> do
          optActivities <- parseGameInput <$> replicateM n getLine
          case optActivities of
            Nothing -> fail "Could not parse activities"
            Just activities -> do
                putStrLn $ show $ length $ activitySelector activities
