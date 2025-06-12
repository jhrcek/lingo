{-# LANGUAGE OverloadedStrings #-}

module Lingo.Repl.Command
    ( Language (..)
    , Command (..)
    , commandP
    , toIso639LanguageCode
    ) where

import Control.Applicative (asum)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, choice, eof, manyTill, (<?>))
import Text.Megaparsec.Char (space1, string)

data Language
    = Portuguese
    | English
    | German
    | Czech
    deriving stock (Show, Eq, Enum, Bounded)

languageUniverse :: [Language]
languageUniverse = [minBound .. maxBound]

toIso639LanguageCode :: Language -> Text
toIso639LanguageCode = \case
    Portuguese -> "pt"
    English -> "en"
    German -> "de"
    Czech -> "cs"

data Command
    = SetLanguage Language
    | Translate Language Text
    | Example Text
    | Define Text
    | More
    | Help
    -- | RawPrompt Text -- TODO implement this, but in a way that we get parse errors for incorrect commands
    | Quit
    deriving stock (Show, Eq)

type Parser = Parsec Void Text

languageP :: Parser Language
languageP =
    asum (fmap (\lang -> string (toIso639LanguageCode lang) $> lang) languageUniverse)
        <?> Text.unpack ("Language: " <> Text.intercalate ", " (toIso639LanguageCode <$> languageUniverse))

restOfLine :: Parser Text
restOfLine =
    Text.pack <$> manyTill anySingle eof

parseTranslate :: Parser Command
parseTranslate = do
    _ <- string ":translate"
    _ <- space1
    sourceLang <- languageP
    _ <- space1
    Translate sourceLang <$> restOfLine

parseExample :: Parser Command
parseExample = do
    _ <- string ":e"
    _ <- space1
    Example <$> restOfLine

parseDefine :: Parser Command
parseDefine = do
    _ <- string ":d" -- TODO make it possible to use :d or :def or :define
    _ <- space1
    Define <$> restOfLine

parseSetLanguage :: Parser Command
parseSetLanguage = do
    _ <- string ":lang"
    _ <- space1
    SetLanguage <$> languageP

parseMore :: Parser Command
parseMore = string ":more" $> More

parseHelp :: Parser Command
parseHelp = string ":help" $> Help

parseQuit :: Parser Command
parseQuit = string ":quit" $> Quit

commandP :: Parser Command
commandP = do
    cmd <-
        choice
            [ parseDefine
            , parseTranslate
            , parseExample
            , parseMore
            , parseHelp
            , parseQuit
            , parseSetLanguage
            ]
    eof
    return cmd