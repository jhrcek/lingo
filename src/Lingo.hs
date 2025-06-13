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
    , model :: Text -- TODO add command to change model
    , messageHistory :: [Message]
    -- ^ History of messages in reverse order
    }

initState :: LingoState
initState =
    LingoState
        { outputLanguage = English
        , model = "gemma3:12b" -- "gemma3:12b"
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

usageMessage :: String
usageMessage =
    unlines
        [ "Available commands:"
        , "  :d, :define <string>               generate a definition of the word/expression <string>"
        , "  :e, :example <string>              generate an example sentence using the word/expression <string>"
        , "  :h, :help                          show this help message"
        , "  :lang <language code>              set the output language (en, pt, de, cs)"
        , "  :q, :quit                          exit the REPL"
        , "  :t, :translate [inlang] <string>   translate <string> to output language - specify input language to disambiguate"
        , "  <RAW_PROMPT>                       send prompt to the underlying LLM as is" -- TODO implement this
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
                    let todo = outputStrLn $ show cmd
                    case cmd of
                        SetLanguage lang ->
                            lift $ modify (\s -> s{outputLanguage = lang})
                        Define userInput ->
                            chatPrompt $ definePrompt outputLang userInput
                        Translate _lang1 _str ->
                            todo
                        Example userInput ->
                            chatPrompt $ examplePrompt outputLang userInput
                        Help ->
                            outputStrLn usageMessage
                        Quit ->
                            pure ()

                    when (cmd /= Quit) repl

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
    resp <- liftIO $ chat ops Nothing
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
    pure ()