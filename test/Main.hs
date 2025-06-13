{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Lingo.Repl.Command
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [commandParserTests]

commandParserTests :: TestTree
commandParserTests =
    testGroup
        "Command Parser Tests"
        [ testGroup "SetLanguage commands" setLanguageTests
        , testGroup "Translate commands" translateTests
        , testGroup "Example commands" exampleTests
        , testGroup "Define commands" defineTests
        , testGroup "Simple commands" simpleCommandTests
        , testGroup "Parse failures" parseFailureTests
        ]

setLanguageTests :: [TestTree]
setLanguageTests =
    [ testCase ":lang pt" $
        parseSuccess ":lang pt" (SetLanguage Portuguese)
    , testCase ":lang en" $
        parseSuccess ":lang en" (SetLanguage English)
    , testCase ":lang de" $
        parseSuccess ":lang de" (SetLanguage German)
    , testCase ":lang cs" $
        parseSuccess ":lang cs" (SetLanguage Czech)
    ]

translateTests :: [TestTree]
translateTests =
    [ testCase ":translate pt hello world" $
        parseSuccess ":translate pt hello world" (Translate Portuguese "hello world")
    , testCase ":t en olá mundo" $
        parseSuccess ":t en olá mundo" (Translate English "olá mundo")
    , testCase ":translate de Hallo Welt" $
        parseSuccess ":translate de Hallo Welt" (Translate German "Hallo Welt")
    , testCase ":t cs ahoj světe" $
        parseSuccess ":t cs ahoj světe" (Translate Czech "ahoj světe")
    ]

exampleTests :: [TestTree]
exampleTests =
    [ testCase ":e word" $ parseSuccess ":e word" (Example "word")
    , testCase ":example multiple words" $ parseSuccess ":example multiple words" (Example "multiple words")
    , testCase ":e word with punctuation!" $ parseSuccess ":e word with punctuation!" (Example "word with punctuation!")
    , testCase ":example complex phrase" $ parseSuccess ":example complex phrase" (Example "complex phrase")
    ]

defineTests :: [TestTree]
defineTests =
    [ testCase ":d word" $
        parseSuccess ":d word" (Define "word")
    , testCase ":define multiple words" $
        parseSuccess ":define multiple words" (Define "multiple words")
    , testCase ":d complex phrase with symbols @#$" $
        parseSuccess ":d complex phrase with symbols @#$" (Define "complex phrase with symbols @#$")
    , testCase ":define another test" $
        parseSuccess ":define another test" (Define "another test")
    ]

simpleCommandTests :: [TestTree]
simpleCommandTests =
    [ testCase ":help" $ parseSuccess ":help" Help
    , testCase ":h" $ parseSuccess ":h" Help
    , testCase ":quit" $ parseSuccess ":quit" Quit
    , testCase ":q" $ parseSuccess ":q" Quit
    ]

parseFailureTests :: [TestTree]
parseFailureTests =
    [ testCase "empty input" $ parseFailure ""
    , testCase "unknown command" $ parseFailure ":unknown"
    , testCase ":lang without argument" $ parseFailure ":lang"
    , testCase ":lang with invalid language" $ parseFailure ":lang xx"
    , testCase ":translate without arguments" $ parseFailure ":translate"
    , testCase ":t without arguments" $ parseFailure ":t"
    , testCase ":translate with invalid language" $ parseFailure ":translate xx hello"
    , testCase ":t with invalid language" $ parseFailure ":t xx hello"
    , testCase ":e without argument" $ parseFailure ":e"
    , testCase ":example without argument" $ parseFailure ":example"
    , testCase ":d without argument" $ parseFailure ":d"
    , testCase ":define without argument" $ parseFailure ":define"
    , testCase "extra text after :help" $ parseFailure ":help extra"
    , testCase "extra text after :h" $ parseFailure ":h extra"
    ]

parseSuccess :: Text -> Command -> Assertion
parseSuccess input expected =
    case parse commandP "test" input of
        Left err -> assertFailure $ "Expected successful parse, but got error: " ++ show err
        Right result -> assertEqual ("Parsing: " ++ show input) expected result

parseFailure :: Text -> Assertion
parseFailure input =
    case parse commandP "test" input of
        Left _ -> return () -- Expected failure
        Right result -> assertFailure $ "Expected parse failure, but got: " ++ show result
