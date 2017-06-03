{-# LANGUAGE ScopedTypeVariables #-}

module War where

import           Control.Monad (replicateM)
-- import           Debug.Trace   (trace)

-- Data Modeling

data Suit = Diamond
          | Club
          | Heart
          | Spade
          deriving (Eq)

instance Show Suit where
  show Diamond = "D"
  show Club    = "C"
  show Heart   = "H"
  show Spade   = "S"

instance Read Suit where
  readsPrec _ (c:cs) = case readCharValue c of
                         Nothing -> []
                         Just s  -> [(s, cs)]
    where readCharValue 'D' = Just Diamond
          readCharValue 'C' = Just Club
          readCharValue 'H' = Just Heart
          readCharValue 'S' = Just Spade
          readCharValue _   = Nothing
  readsPrec _ _ = []

data Value = Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Ten
           | Jake
           | Queen
           | King
           | Ace
           deriving (Eq, Ord)

instance Show Value where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"
  show Jake  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

instance Read Value where
  readsPrec _ ('1':'0':cs) = [(Ten, cs)]
  readsPrec _ (c:cs) = case readCharValue c of
                         Nothing  -> []
                         Just val -> [(val, cs)]
    where readCharValue '2' = Just Two
          readCharValue '3' = Just Three
          readCharValue '4' = Just Four
          readCharValue '5' = Just Five
          readCharValue '6' = Just Six
          readCharValue '7' = Just Seven
          readCharValue '8' = Just Eight
          readCharValue '9' = Just Nine
          readCharValue 'J' = Just Jake
          readCharValue 'Q' = Just Queen
          readCharValue 'K' = Just King
          readCharValue 'A' = Just Ace
          readCharValue _   = Nothing
  readsPrec _ _ = []

data Card = Card { suit :: Suit, value :: Value } deriving (Eq)

instance Show Card where
  show (Card s v) = show v ++ show s

instance Ord Card where
  compare (Card _ value1) (Card _ value2) = compare value1 value2

newtype Deck = Deck [Card] deriving (Eq, Show)

newtype BattleDecks = BattleDecks (Deck, Deck) deriving (Eq, Show)

initBattleDecks :: BattleDecks
initBattleDecks = BattleDecks (Deck [], Deck [])

data Player = Player1 | Player2 deriving (Eq)
data GameResult = Winner Int Player | Tie deriving (Eq)

instance Show GameResult where
  show Tie                = "PAT"
  show (Winner k Player1) = "1 " ++ show k
  show (Winner k Player2) = "2 " ++ show k

-- Parsers

parseValue :: String -> Maybe Value
parseValue string =
    case reads string :: [(Value, String)] of
      [(val, _)] -> Just val
      _          -> Nothing

parseSuit :: String -> Maybe Suit
parseSuit string =
    case reads string :: [(Suit, String)] of
      [(s, _)] -> Just s
      _        -> Nothing

parseCard :: String -> Maybe Card
parseCard string = do
  su <- ((:[]) <$> safeLast string) >>= parseSuit
  v <- parseValue $ reverse $ drop 1 $ reverse string
  return $ Card su v

parseDeck :: [String] -> Maybe Deck
parseDeck cards = Deck <$> traverse parseCard cards

-- TODO: make it safe
parseDecks :: [String] -> Maybe (Deck, Deck)
parseDecks (x:content) = do
  let (n :: Int) = read x
  d1 <- parseDeck (take n content)
  d2 <- parseDeck (drop (n + 1) content)
  return (d1, d2)
parseDecks _ = Nothing

parseGameResult :: String -> Maybe GameResult
parseGameResult string =
  case words string of
    ["PAT"]  -> Just Tie
    ["1", k] -> Just $ Winner (read k) Player1
    ["2", k] -> Just $ Winner (read k) Player2
    _        -> Nothing

-- Game Logic

incGameTurn :: Int -> BattleDecks -> Int
incGameTurn k (BattleDecks (Deck b1, _)) = if null b1 then k + 1 else k

warGameTurn :: (Int, BattleDecks, Deck, Deck) -> Either (Maybe GameResult) (Int, BattleDecks, Deck, Deck)
warGameTurn (k, (BattleDecks (Deck [], Deck [])), Deck [], Deck (_:_)) = Left $ Just $ Winner k Player2
warGameTurn (k, (BattleDecks (Deck [], Deck [])), Deck (_:_), Deck []) = Left $ Just $ Winner k Player1
warGameTurn (k, bds@(BattleDecks (Deck b1, Deck b2)), Deck p1, Deck p2)
  | length b1 /= length b2 = Left $ Just Tie
  | null p1 || null p2     = Left Nothing
  | otherwise              =
    case compare x y of
      GT -> Right (incGameTurn k bds, BattleDecks (Deck [], Deck []), Deck (xs ++ b1 ++ [x] ++ b2 ++ [y]), Deck ys)
      LT -> Right (incGameTurn k bds, BattleDecks (Deck [], Deck []), Deck xs, Deck (ys ++ b1 ++ [x] ++ b2 ++ [y]))
      EQ -> Right ( incGameTurn k bds
                  , BattleDecks (Deck (b1 ++ (take 4 p1))
                  , Deck (b2 ++ (take 4 p2)))
                  , Deck (drop 4 p1)
                  , Deck (drop 4 p2))
    where (x:xs) = p1
          (y:ys) = p2

warGame :: Int -> BattleDecks -> Deck -> Deck -> Maybe GameResult
warGame k battleDecks d1 d2 =
  -- Debug.Trace.trace (showTurn (k, battleDecks, d1, d2)) $
  case warGameTurn (k, battleDecks, d1, d2) of
    Left Nothing                       -> Nothing
    Left (Just gameResult)             -> Just gameResult
    Right (k', battleDecks', d1', d2') -> warGame k' battleDecks' d1' d2'

showTurn :: (Int, BattleDecks, Deck, Deck) -> String
showTurn (k, BattleDecks (b1@(Deck b1'), b2@(Deck b2')), d1@(Deck d1'), d2@(Deck d2')) =
  "Turn#: " ++ show k ++ "\n" ++
  "player1 [" ++ show (length d1') ++ "]: " ++ show d1 ++ "\n" ++
  "player2 [" ++ show (length d2') ++ "]: " ++ show d2 ++ "\n" ++
  "b1 [" ++ show (length b1') ++ "]: " ++ show b1 ++ "\n" ++
  "b2 [" ++ show (length b2') ++ "]: " ++ show b2 ++ "\n"

fight :: Deck -> Deck -> Maybe GameResult
fight = warGame 0 initBattleDecks

gameInput :: IO (Maybe (Deck, Deck))
gameInput = do
    mdeck1 <- getDeck
    mdeck2 <- getDeck
    case (mdeck1, mdeck2) of
      (Just d1, Just d2) -> return $ Just (d1, d2)
      _                  -> return Nothing
    -- TODO: make it safe
    where getDeck :: IO (Maybe Deck)
          getDeck = do
            k :: Int <- read <$> getLine
            parseDeck <$> replicateM k getLine

main :: IO ()
main = do
  mdecks <- gameInput
  case mdecks of
    Nothing -> return ()  -- Parsing failed
    Just (d1, d2) -> case fight d1 d2 of
                       Nothing         -> return () -- Game failed
                       Just gameResult -> print gameResult

-- Utils

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast = safeHead . reverse
