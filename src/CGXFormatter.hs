{-# LANGUAGE ScopedTypeVariables #-}

module CGXFormatter where

import           Control.Applicative
import qualified Data.List                     as L
-- import qualified Debug.Trace                   as T
import           System.IO
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.PrettyPrint              as PP

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

parseStr :: P.Parser String
parseStr = P.between (P.char '\'') (P.char '\'') $
           P.many (P.noneOf "\'")

parseString :: P.Parser Primitive
parseString = S <$> parseStr

parsePrimitive :: P.Parser CGXValue
parsePrimitive = CGXPrimitive <$>
               (parseNull <|>
                parseBool <|>
                parseNumber <|>
                parseString)

parseBlock :: P.Parser CGXValue
parseBlock = do
    _ <- P.char '('
    P.skipMany P.space
    values <- P.sepBy parseCGXValue (P.char ';')
    P.skipMany P.space
    _ <- P.char ')'
    return (CGXBlock values)

parseKeyValue :: P.Parser CGXValue
parseKeyValue = do
    key <- parseStr
    _ <- P.char '='
    value <- parseCGXValue
    return $ CGXKeyValue key value

parseCGXValue :: P.Parser CGXValue
parseCGXValue =
  P.skipMany P.newline *>
  P.skipMany P.tab *>
  P.skipMany P.space *>
    (P.try parseKeyValue <|>
      parseBlock <|>
      parsePrimitive)

readCGX :: String -> Maybe CGXValue
readCGX str = case P.parse parseCGXValue "cgx" str of
                Left _  -> Nothing
                Right v -> Just v

readCGX' :: String -> Either P.ParseError CGXValue
readCGX' = P.parse parseCGXValue "cgx"

-- Pretty Printer

indent :: Int -> PP.Doc -> PP.Doc
indent k doc = PP.hsep $ (take k $ L.repeat (PP.text "")) ++ [doc]

pprintHelper :: Int -> CGXValue -> PP.Doc
pprintHelper k (CGXPrimitive Null)      = indent k $ PP.text "null"
pprintHelper k (CGXPrimitive (B True))  = indent k $ PP.text "true"
pprintHelper k (CGXPrimitive (B False)) = indent k $ PP.text "false"
pprintHelper k (CGXPrimitive (N l))     = indent k $ PP.int l
pprintHelper k (CGXPrimitive (S str))   = indent k $ PP.quotes $ PP.text str
pprintHelper k (CGXBlock [])            = indent k (PP.lparen PP.<> PP.text "\n") PP.<> indent k PP.rparen
pprintHelper k (CGXBlock xs)            =
    indent k PP.lparen PP.<>
    PP.hcat (PP.punctuate PP.semi $ elements xs) PP.<>
    PP.text "\n" PP.<> (indent k PP.rparen)
  where
      elements :: [CGXValue] -> [PP.Doc]
      elements = fmap (\e -> (PP.text "\n" PP.<> (pprintHelper (k + 4) e)))
pprintHelper k (CGXKeyValue key val@(CGXBlock _)) =
    indent k (PP.quotes (PP.text key) PP.<> PP.equals) PP.<>
    PP.text "\n" PP.<> pprintHelper k val
pprintHelper k (CGXKeyValue key val)              =
    indent k (PP.quotes (PP.text key) PP.<> PP.equals) PP.<>
    pprintHelper 0 val

pprint :: CGXValue -> PP.Doc
pprint = pprintHelper 0

-- Game Logic

debugGameInput :: CGXValue -> IO ()
debugGameInput cgx = do
  hPutStrLn stderr $ "Parsed CGX Value: " ++ show cgx
  hPutStrLn stderr $ "Pretty Printed CGX Value: " ++ show (pprint cgx)

gameInput :: IO (Maybe CGXValue)
gameInput = getLine >> fmap readCGX getContents

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    mcgx <- gameInput
    case mcgx of
      Nothing  -> fail "could not parse cgx file"
      Just cgx -> debugGameInput cgx >> print (pprint cgx)
