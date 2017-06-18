{-# LANGUAGE ScopedTypeVariables #-}

module CGXFormatter where

import           Control.Applicative
import           Control.Monad
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes, isJust)
import qualified Debug.Trace                   as T
import           Safe                          (atMay, headMay)
import           System.IO
import qualified Text.ParserCombinators.Parsec as P

-- Data Modeling

data Primitive = N Int
               | B Bool
               | Null
               | S String
               deriving (Eq, Show)

data CGXValue = CGXPrimitive Primitive
              | CGXBlock [CGXValue]
              | CGXKeyValue String CGXValue
              deriving (Eq, Show)

-- Parsers

parseNull :: P.Parser Primitive
parseNull = P.string "null" *> return Null

parseNumber :: P.Parser Primitive
parseNumber = N . read <$> P.many1 P.digit

parseTrue :: P.Parser Primitive
parseTrue = P.string "true" *> return (B True)

parseFalse :: P.Parser Primitive
parseFalse = P.string "false" *> return (B False)

parseBool :: P.Parser Primitive
parseBool = parseTrue <|> parseFalse

parseString :: P.Parser Primitive
parseString = P.between (P.char '\'') (P.char '\'') $
              S <$> P.many P.letter


parsePrimitive :: P.Parser CGXValue
parsePrimitive = CGXPrimitive <$>
               (parseNull <|>
                parseBool <|>
                parseNumber <|>
                parseString)

parseBlock :: P.Parser CGXValue
parseBlock = do
    _ <- P.char '('
    values <- P.sepBy parseCGXValue (P.char ';')
    _ <- P.char ')'
    return (CGXBlock values)

parseKeyValue :: P.Parser CGXValue
parseKeyValue = do
    key <- P.many P.letter
    _ <- P.char '='
    value <- parseCGXValue
    return $ CGXKeyValue key value

parseCGXValue :: P.Parser CGXValue
parseCGXValue = parsePrimitive <|>
                parseBlock <|>
                parseKeyValue

readCGX :: String -> Maybe CGXValue
readCGX str = case P.parse parseCGXValue "cgx" str of
                Left _  -> Nothing
                Right v -> Just v

-- Pretty Printer


-- Game Logic
