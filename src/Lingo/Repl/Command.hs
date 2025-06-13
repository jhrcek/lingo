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
import Text.Megaparsec (Parsec, anySingle, choice, eof, manyTill, notFollowedBy, some, (<?>))
import Text.Megaparsec.Char (char, space1, string)

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
    | Help
    | RawPrompt Text
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
    _ <- choice [string ":translate", string ":t"]
    _ <- space1
    sourceLang <- languageP
    _ <- space1
    Translate sourceLang <$> restOfLine

parseExample :: Parser Command
parseExample = do
    _ <- choice [string ":example", string ":e"]
    _ <- space1
    Example <$> restOfLine

parseDefine :: Parser Command
parseDefine = do
    _ <- choice [string ":define", string ":d"]
    _ <- space1
    Define <$> restOfLine

parseSetLanguage :: Parser Command
parseSetLanguage = do
    _ <- string ":lang"
    _ <- space1
    SetLanguage <$> languageP

parseHelp :: Parser Command
parseHelp = choice [string ":help", string ":h"] $> Help

parseQuit :: Parser Command
parseQuit = choice [string ":quit", string ":q"] $> Quit

parseRawPrompt :: Parser Command
parseRawPrompt = do
    notFollowedBy (char ':')
    content <- Text.pack <$> some anySingle
    eof
    pure $ RawPrompt content

commandP :: Parser Command
commandP = do
    cmd <-
        choice
            [ parseDefine
            , parseTranslate
            , parseExample
            , parseHelp
            , parseQuit
            , parseSetLanguage
            , parseRawPrompt
            ]
    eof
    return cmd
