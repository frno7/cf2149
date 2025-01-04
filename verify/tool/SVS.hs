{-# LANGUAGE RecordWildCards #-}
-- SPDX-License-Identifier: GPL-2.0
-- Copyright (C) 2025 Fredrik Noring

module SVS
    ( fromSvs
    ) where

import Data.Char (toUpper)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Signal

type Parser = Parsec Void String

type Value = Integer

lexeme  = L.lexeme hspace
integer = lexeme $       L.decimal
       <|> (char' 'b' >> L.binary)
       <|> (char' 'o' >> L.octal)
       <|> (char' 'd' >> L.decimal)
       <|> (char' 'x' >> L.hexadecimal)
logic = integer

identifier  = lexeme $ (some (alphaNumChar <|> char '_'))
identifiers = lexeme $ (some (alphaNumChar <|> char '_')) `sepBy` char '/'

parseTime :: Parser Time
parseTime = do
    timeCycle <-                       integer
    timeDelta <- char '@' >> hspace >> integer
    timeUnit  <- read <$> fmap toUpper <$> identifier
    return $ Time {..}

parseSignalValue :: Parser (SignalValue (Signal [String]) Value)
parseSignalValue = do
    signalName  <-             identifiers
    signalWidth <- char '.' >> fromInteger <$> integer
    value       <- char '=' >> logic
    return $ SignalValue Signal {..} value

parseSignalValues :: Parser [SignalValue (Signal [String]) Value]
parseSignalValues = many parseSignalValue

parseEntry :: Parser (Event [String] [SignalValue (Signal [String]) Value])
parseEntry = do
    eventName <- char '/'           >> identifiers
    eventTime <- char '#' >> hspace >> parseTime
    event     <- char ':' >> hspace >> parseSignalValues
    eol >> return Event {..}

parseEntries :: Parser [Event [String] [SignalValue (Signal [String]) Value]]
parseEntries = do
    entryLines <- many parseEntry
    eof >> return entryLines

fromSvs :: String
        -> Either String [Event [String] [(SignalValue (Signal [String]) Value)]]
fromSvs s = case parse parseEntries "" s of
    Left  e -> Left $ errorBundlePretty e
    Right e -> Right e
