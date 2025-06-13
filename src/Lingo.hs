{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lingo where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, gets, modify)
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Ollama.Chat
import Data.Ollama.List (ModelInfo (name), Models (Models), listM)
import Data.Ollama.Load (loadGenModelM)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Lingo.Repl.Command
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty, parse)

type Lingo a = InputT (StateT LingoState IO) a

data LingoState = LingoState
    { outputLanguage :: Language
    , model :: Text
    , messageHistory :: [Message]
    -- ^ History of messages in reverse order
    }

initState :: LingoState
initState =
    LingoState
        { outputLanguage = English
        , model = "gemma3:12b"
        , messageHistory = [systemPrompt]
        }

systemPrompt :: Message
systemPrompt =
    systemMessage
        ( Text.unwords
            [ "You're a language assistant that answers user's questions in the language that user asks in."
            , "Rules:"
            , "1. only outputs what user explicitly asks for."
            , "2. never use emojis."
            , "3. never ask followup questions."
            , "4. if user asks for an example or definition of a word they already asked for, come up with a new example or definition."
            ]
        )

examplePrompt :: Language -> Text -> Message
examplePrompt language input = userMessage $ case language of
    Portuguese -> "Dê-me uma frase de exemplo que use a palavra '" <> input <> "'. Responda em português."
    English -> "Give me an example sentence that uses the word '" <> input <> "'. Answer in English."
    German -> "Gib mir einen Beispielsatz, der das Wort '" <> input <> "' verwendet. Antworte auf Deutsch."
    Czech -> "Dej mi příklad věty, která používá slovo '" <> input <> "'. Odpověz česky."

definePrompt :: Language -> Text -> Message
definePrompt language input = userMessage $ case language of
    -- TODO parse input and adjust prompt by saing word/expression
    English -> "Give me a definition of the word '" <> input <> "' in about 1-2 sentences (only use 2 if necessary for clarity). The definition should be a substantive phrase, doesn't need to be a full sentence. Answer in English."
    Portuguese -> "Dê-me uma definição da palavra '" <> input <> "' em cerca de 1-2 frases. Não repita a palavra que está sendo definida. Responda em português."
    German -> "Definiere das Wort '" <> input <> "' in etwa 1-2 Sätzen. Wiederhole das definierte Wort nicht. Antworte auf Deutsch."
    Czech -> "Definuj slovo '" <> input <> "' v přibližně 1-2 větách. Neopakuj definované slovo. Odpověz česky."

translatePrompt :: Language -> Language -> Text -> Maybe Message
translatePrompt inLang outLang input
    | inLang == outLang = Nothing
    | otherwise = Just $ userMessage prompt
  where
    isWord = length (Text.words input) == 1
    sourceLangName = languageNameIn inLang outLang
    wordOrPhrase = wordOrPhraseIn outLang

    wordOrPhraseIn :: Language -> Text
    wordOrPhraseIn lang = case lang of
        Portuguese -> if isWord then "palavra" else "expressão"
        English -> if isWord then "word" else "phrase"
        German -> if isWord then "Wort" else "Ausdruck"
        Czech -> if isWord then "slovo" else "výraz"

    languageNameIn :: Language -> Language -> Text
    languageNameIn sourceLang targetLang = case targetLang of
        Portuguese -> case sourceLang of
            Portuguese -> "português"
            English -> "inglesa"
            German -> "alemã"
            Czech -> "checa"
        English -> case sourceLang of
            Portuguese -> "Portuguese"
            English -> "English"
            German -> "German"
            Czech -> "Czech"
        German -> case sourceLang of
            Portuguese -> "portugiesische"
            English -> "englische"
            German -> "deutsche"
            Czech -> "tschechische"
        Czech -> case sourceLang of
            Portuguese -> "portugalské"
            English -> "anglické"
            German -> "německé"
            Czech -> "české"

    prompt = case outLang of
        Portuguese -> "Traduza a " <> wordOrPhrase <> " " <> sourceLangName <> " '" <> input <> "' para o português."
        English -> "Translate the " <> sourceLangName <> " " <> wordOrPhrase <> " '" <> input <> "' to English."
        German -> "Übersetze das " <> sourceLangName <> " " <> wordOrPhrase <> " '" <> input <> "' ins Deutsche."
        Czech -> "Přelož " <> sourceLangName <> " " <> wordOrPhrase <> " '" <> input <> "' do češtiny."

usageMessage :: String
usageMessage =
    unlines
        [ "Available commands:"
        , "  :d, :define <string>               generate a definition of the word/expression <string>"
        , "  :e, :example <string>              generate an example sentence using the word/expression <string>"
        , "  :h, :help                          show this help message"
        , "  :q, :quit                          exit the REPL"
        , "  :set lang <language code>          set the output language (en, pt, de, cs)"
        , "  :set model                         interactive model selection"
        , "  :t, :translate [inlang] <string>   translate <string> to output language - specify input language to disambiguate"
        , "  <RAW_PROMPT>                       send prompt to the underlying LLM as is"
        ]

appendHistory :: Message -> Lingo ()
appendHistory msg =
    lift $ modify (\s -> s{messageHistory = msg : messageHistory s})

getHistory :: Lingo (NonEmpty Message)
getHistory = do
    msgs <- lift $ gets messageHistory
    pure $ case NE.nonEmpty (List.reverse msgs) of
        Just nonEmptyMsgs -> nonEmptyMsgs
        Nothing -> systemPrompt NE.:| []

repl :: Lingo ()
repl = do
    outputLang <- lift $ gets outputLanguage
    minput <- getInputLine $ Text.unpack $ toIso639LanguageCode outputLang <> "> "
    case minput of
        Nothing -> return ()
        Just input -> do
            case parse commandP "user input" (Text.pack input) of
                Left err -> do
                    outputStrLn $ errorBundlePretty err
                    repl
                Right cmd -> do
                    case cmd of
                        Set setting -> case setting of
                            SetLanguage lang ->
                                lift $ modify (\s -> s{outputLanguage = lang})
                            SetModel -> do
                                selectedModel <- selectModelInteractively
                                case selectedModel of
                                    Just model -> do
                                        lift $ modify $ \s -> s{Lingo.model = model}
                                        outputStrLn $ "Loading model " ++ Text.unpack model ++ " ..."
                                        _ <- loadGenModelM model
                                        outputStrLn "Loaded"
                                    Nothing ->
                                        outputStrLn "Model selection cancelled."
                        Define userInput ->
                            chatPrompt $ definePrompt outputLang userInput
                        Translate inLang userInput ->
                            case translatePrompt inLang outputLang userInput of
                                Nothing ->
                                    outputStrLn "No translation needed, input language is the same as output language."
                                Just prompt ->
                                    chatPrompt prompt
                        Example userInput ->
                            chatPrompt $ examplePrompt outputLang userInput
                        RawPrompt userInput ->
                            chatPrompt $ userMessage userInput
                        Help ->
                            outputStrLn usageMessage
                        Quit ->
                            pure ()

                    when (cmd /= Quit) repl

selectModelInteractively :: Lingo (Maybe Text)
selectModelInteractively = do
    resp <- listM Nothing
    case resp of
        Left err -> do
            outputStrLn $ "Error listing models: " ++ show err
            pure Nothing
        Right (Models models)
            | null models -> do
                outputStrLn "No models available."
                pure Nothing
            | otherwise -> do
                let modelNames = List.sort $ fmap name models
                outputStrLn "Available models:"
                for_ (zip [1 :: Int ..] modelNames) $ \(i, modelName) ->
                    outputStrLn $ "  " ++ show i ++ ") " ++ Text.unpack modelName
                let promptForChoice = do
                        minput <- getInputLine "Enter model number (or press Enter to cancel): "
                        case minput of
                            Nothing -> pure Nothing
                            Just "" -> pure Nothing
                            Just input ->
                                case reads input of
                                    [(n, "")]
                                        | 0 < n && n <= length modelNames ->
                                            pure $ Just $ modelNames !! (n - 1)
                                    _ -> do
                                        outputStrLn $ "Invalid choice. Please enter a number between 1 and " ++ show (length models) ++ "."
                                        promptForChoice
                promptForChoice

chatPrompt :: Message -> Lingo ()
chatPrompt userMsg = do
    model <- lift $ gets Lingo.model
    appendHistory userMsg
    msgs <- getHistory
    -- A workaround to allow both printing each response chunk as it comes in, while still allowing
    -- appending the complete final response to the history as a single message.
    responseChunksRef <- liftIO $ newIORef []
    let ops =
            defaultChatOps
                { chatModelName = model
                , messages = msgs
                , stream =
                    Just
                        ( \resp -> do
                            for_ (message resp) $ \msg -> do
                                Text.putStr $ content msg
                                modifyIORef' responseChunksRef (content msg :)
                            when (done resp) $ Text.putStrLn ""
                        , hFlush stdout
                        )
                }
    resp <- chatM ops Nothing
    case resp of
        Left err -> outputStrLn $ "Error: " ++ show err
        Right chatResponse ->
            case message chatResponse of
                Nothing ->
                    outputStrLn $ Text.unpack $ "Oops, " <> model <> " didn't provide any response." -- TODO handle this better?
                Just msg -> do
                    responseChunks <- liftIO $ readIORef responseChunksRef
                    appendHistory msg{content = Text.concat $ List.reverse responseChunks}

main :: IO ()
main = do
    _ <- execStateT (runInputT defaultSettings repl) initState
    -- TODO load model here
    pure ()
