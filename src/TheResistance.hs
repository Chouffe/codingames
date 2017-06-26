{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TheResistance where

import           Control.Applicative
import           Control.Monad.Trans.State.Strict
import qualified Data.Map                         as M
import           Data.Maybe                       (mapMaybe)
import qualified Data.Text                        as T
import           System.IO

-- Data Modeling

type Plain = String
type PlainChar = Char
type Morse = T.Text
type MorseTable = M.Map PlainChar Morse
type Dict = M.Map Morse Integer  -- TODO: Further improvement: Use a Trie to get prefixes in O(lg n)
type Memo = M.Map Morse Integer

-- API

emptyDict :: Dict
emptyDict = M.empty

insertDict :: MorseTable -> Plain -> Dict -> Dict
insertDict mt word d =
  case plainToMorse mt word of
    Nothing -> d
    Just m  -> M.insertWith (+) m 1 d

buildDict :: Foldable t => MorseTable -> t Plain -> Dict
buildDict mt = foldr (insertDict mt) emptyDict

morseTable :: MorseTable
morseTable = M.fromList [ ('A', ".-")
                        , ('B', "-...")
                        , ('C', "-.-.")
                        , ('D', "-..")
                        , ('E', ".")
                        , ('F', "..-.")
                        , ('G', "--.")
                        , ('H', "....")
                        , ('I', "..")
                        , ('J', ".---")
                        , ('K', "-.-")
                        , ('L', ".-..")
                        , ('M', "--")
                        , ('N', "-.")
                        , ('O', "---")
                        , ('P', ".--.")
                        , ('Q', "--.-")
                        , ('R', ".-.")
                        , ('S', "...")
                        , ('T', "-")
                        , ('U', "..-")
                        , ('V', "...-")
                        , ('W', ".--")
                        , ('X', "-..-")
                        , ('Y', "-.--")
                        , ('Z', "--..")
                        ]

plainToMorse :: MorseTable -> Plain -> Maybe Morse
plainToMorse _ []  = Nothing
plainToMorse mt [c] = lookupMorseTable c mt
plainToMorse mt (c:cs) = do
  m <- lookupMorseTable c mt
  ms <- plainToMorse mt cs
  return (T.append m ms)

lookupMorseTable :: PlainChar -> MorseTable -> Maybe Morse
lookupMorseTable = M.lookup

-- Parsers

parseMorse :: String -> Maybe Morse
parseMorse = Just . T.pack

parsePlain :: [String] -> Maybe [Plain]
parsePlain = Just

parseGameInput :: String -> Maybe (Morse, [Plain])
parseGameInput content = case lines content of
                           (m:_:xs) -> liftA2 (,) (parseMorse m) (parsePlain xs)
                           _        -> Nothing


-- Game Logic

-- Non memoized solution: does not scale on large inputs
messageNumber :: Dict -> Morse -> Integer
messageNumber _ "" = 1
messageNumber d m  = sum $ map (\(_, s, l) -> l * messageNumber d s) $ getPrefixesAndSuffixes d m

-- Memoized solution
memoizedMessageNumber :: Dict -> Morse -> State Memo Integer
memoizedMessageNumber d m = do
  memo <- get
  case M.lookup m memo of
    Just n  -> return n
    Nothing -> do
      ns <- traverse (\(_, s, l) -> (*l) <$> memoizedMessageNumber d s) (getPrefixesAndSuffixes d m)
      modify (M.insert m (sum ns))
      return (sum ns)

runMemoizedMessageNumber :: Dict -> Morse -> Integer
runMemoizedMessageNumber d m = evalState (memoizedMessageNumber d m) (M.fromList [("", 1)])

getPrefixes :: Dict -> Morse -> [(Morse, Integer)]
getPrefixes d m = M.toList $ M.filterWithKey (\k -> const (T.isPrefixOf k m)) d

getPrefixesAndSuffixes :: Dict -> Morse -> [(Morse, Morse, Integer)]
getPrefixesAndSuffixes d m =
  mapMaybe (\(p, n) -> case T.stripPrefix p m of
                            Nothing -> Nothing
                            Just s  -> Just (p, s, n)) prefixes
   where
     prefixes :: [(Morse, Integer)]
     prefixes = getPrefixes d m

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- getContents
    case parseGameInput input of
      Nothing -> fail "Could not parse Game Input"
      Just (morse, ws) -> do
        debugMain morse ws
        print (runMemoizedMessageNumber (buildDict morseTable ws) morse)

-- Debug Logic

debugMain :: Morse -> [Plain] -> IO ()
debugMain morse ws = do
  hPutStrLn stderr $ "morse: " ++ T.unpack morse
  hPutStrLn stderr "words:"
  mapM_ (hPutStrLn stderr) ws
