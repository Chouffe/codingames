{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TheResistance where

import           Control.Applicative
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
-- import qualified Debug.Trace         as T

-- Data Modeling

type Plain = String
type PlainChar = Char
type Morse = T.Text
type MorseTable = M.Map PlainChar Morse
data Dictionary = Dictionary { regular :: S.Set Morse, reversed :: S.Set Morse } deriving (Eq, Show)

-- API

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

insert :: MorseTable -> Plain -> Dictionary -> Dictionary
insert mt word dict =
  case plainToMorse mt word of
    Nothing -> dict
    Just m  -> Dictionary (S.insert m (regular dict))
                          (S.insert (T.reverse m) (reversed dict))

emptyDictionary :: Dictionary
emptyDictionary = Dictionary S.empty S.empty

dictionary :: Foldable t => MorseTable -> t Plain -> Dictionary
dictionary mt = foldr (insert mt) emptyDictionary

elemDictionary :: Morse -> Dictionary -> Bool
elemDictionary = prefix

prefix :: Morse -> Dictionary -> Bool
prefix m dict = S.member m (regular dict)

suffix :: Morse -> Dictionary -> Bool
suffix m dict = S.member (T.reverse m) (reversed dict)

longestPrefix :: Morse -> Dictionary -> Maybe Morse
longestPrefix m dict = L.find (`prefix` dict) (reverse (T.inits m))

longestSuffix :: Morse -> Dictionary -> Maybe Morse
longestSuffix m dict = L.find (`suffix` dict) (T.tails m)

-- TODO: find a combinator that handles that pattern
longestPrefixAndSuffix :: Morse -> Dictionary -> Maybe (Morse, Morse)
longestPrefixAndSuffix m dict = liftA2 (,) (longestPrefix m dict) (longestSuffix m dict)

deleteDictionary :: Morse -> Dictionary -> Dictionary
deleteDictionary m dict = Dictionary (S.delete m (regular dict))
                                     (S.delete (T.reverse m) (reversed dict))

deleteLengthDictionary :: Int -> Dictionary -> Dictionary
deleteLengthDictionary l dict = Dictionary (removeLength (regular dict))
                                           (removeLength (reversed dict))
  where
    removeLength :: S.Set Morse -> S.Set Morse
    removeLength = S.filter ((< l) . T.length)

deleteWordAndLengthDictionary :: Morse -> Dictionary -> Dictionary
deleteWordAndLengthDictionary m dict = deleteLengthDictionary (T.length m + 1) (deleteDictionary m dict)

messageNumber :: Morse -> Dictionary -> Int
messageNumber m dict
  | elemDictionary m dict = 1 + messageNumber m (deleteDictionary m dict)
  | otherwise = case mviews m dict of
                  Nothing -> 0
                  Just (pr, prs, su, sus) ->
                    (1 + messageNumber pr (deleteWordAndLengthDictionary pr dict)) *
                      messageNumber prs (deleteLengthDictionary (T.length prs + 1) dict) +
                    (1 + messageNumber su (deleteWordAndLengthDictionary su dict)) *
                      messageNumber sus (deleteLengthDictionary (T.length sus + 1) dict)
      where
        mviews :: Morse -> Dictionary -> Maybe (T.Text, T.Text, T.Text, T.Text)
        mviews morse d = do
          (pr, su) <- longestPrefixAndSuffix morse d
          prs <- T.stripPrefix pr morse
          sus <- T.stripSuffix su morse
          return (pr, prs, su, sus)

-- Tests

tests :: [(Morse, Int)]
tests = [ ("-.-", 1)
        , ("--.-------..", 1)
        , ("......-...-..---.-----.-..-..-..", 2)
        ]

mdictionary1 :: Dictionary
mdictionary1 = dictionary morseTable ["A", "B", "C", "HELLO", "K", "WORLD"]

mdictionary2 :: Dictionary
mdictionary2 = dictionary morseTable ["GOD", "GOOD", "MORNING", "G", "HELLO"]

mdictionary3 :: Dictionary
mdictionary3 = dictionary morseTable ["HELL", "HELLO", "OWRLD", "WORLD","TEST"]
