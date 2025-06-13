{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Lingo.Repl.Command
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

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
        , testGroup "Raw prompt commands" rawPromptTests
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

rawPromptTests :: [TestTree]
rawPromptTests =
    [ testCase "simple raw prompt" $
        parseSuccess "hello world" (RawPrompt "hello world")
    , testCase "raw prompt with numbers" $
        parseSuccess "Tell me about 42" (RawPrompt "Tell me about 42")
    , testCase "raw prompt with special characters" $
        parseSuccess "What about @#$%?" (RawPrompt "What about @#$%?")
    , testCase "single character raw prompt" $
        parseSuccess "a" (RawPrompt "a")
    ]

parseFailureTests :: [TestTree]
parseFailureTests =
    [ testCase "empty input" $
        parseFailure
            ""
            """
            test:1:1:
              |
            1 | <empty line>
              | ^
            unexpected end of input
            expecting ":d", ":define", ":e", ":example", ":h", ":help", ":lang", ":q", ":quit", ":t", or ":translate"
            """
    , testCase "unknown command" $
        parseFailure
            ":unknown"
            """
            test:1:1:
              |
            1 | :unknown
              | ^^^^^^^^
            unexpected ":unknown"
            expecting ":d", ":define", ":e", ":example", ":h", ":help", ":lang", ":q", ":quit", ":t", or ":translate"
            """
    , testCase ":lang without argument" $
        parseFailure
            ":lang"
            """
            test:1:6:
              |
            1 | :lang
              |      ^
            unexpected end of input
            expecting white space
            """
    , testCase ":lang with invalid language" $
        parseFailure
            ":lang xx"
            """
            test:1:7:
              |
            1 | :lang xx
              |       ^^
            unexpected "xx"
            expecting Language: pt, en, de, cs or white space
            """
    , testCase ":translate without arguments" $
        parseFailure
            ":translate"
            """
            test:1:11:
              |
            1 | :translate
              |           ^
            unexpected end of input
            expecting white space
            """
    , testCase ":t without arguments" $
        parseFailure
            ":t"
            """
            test:1:3:
              |
            1 | :t
              |   ^
            unexpected end of input
            expecting white space
            """
    , testCase ":translate with invalid language" $
        parseFailure
            ":translate xx hello"
            """
            test:1:12:
              |
            1 | :translate xx hello
              |            ^^
            unexpected "xx"
            expecting Language: pt, en, de, cs or white space
            """
    , testCase ":t with invalid language" $
        parseFailure
            ":t xx hello"
            """
            test:1:4:
              |
            1 | :t xx hello
              |    ^^
            unexpected "xx"
            expecting Language: pt, en, de, cs or white space
            """
    , testCase ":e without argument" $
        parseFailure
            ":e"
            """
            test:1:3:
              |
            1 | :e
              |   ^
            unexpected end of input
            expecting white space
            """
    , testCase ":example without argument" $
        parseFailure
            ":example"
            """
            test:1:9:
              |
            1 | :example
              |         ^
            unexpected end of input
            expecting white space
            """
    , testCase ":d without argument" $
        parseFailure
            ":d"
            """
            test:1:3:
              |
            1 | :d
              |   ^
            unexpected end of input
            expecting white space
            """
    , testCase ":define without argument" $
        parseFailure
            ":define"
            """
            test:1:8:
              |
            1 | :define
              |        ^
            unexpected end of input
            expecting white space
            """
    , testCase "extra text after :help" $
        parseFailure
            ":help extra"
            """
            test:1:6:
              |
            1 | :help extra
              |      ^
            unexpected space
            expecting end of input
            """
    , testCase "extra text after :h" $
        parseFailure
            ":h extra"
            """
            test:1:3:
              |
            1 | :h extra
              |   ^
            unexpected space
            expecting end of input
            """
    , testCase "colon only" $
        parseFailure
            ":"
            """
            test:1:1:
              |
            1 | :
              | ^
            unexpected ':'
            expecting ":d", ":define", ":e", ":example", ":h", ":help", ":lang", ":q", ":quit", ":t", or ":translate"
            """
    ]

parseSuccess :: Text -> Command -> Assertion
parseSuccess input expected =
    case parse commandP "test" input of
        Left err -> assertFailure $ "Expected successful parse, but got error: " ++ show err
        Right result -> assertEqual ("Parsing: " ++ show input) expected result

parseFailure :: Text -> String -> Assertion
parseFailure input expectedErr =
    case parse commandP "test" input of
        Left e -> errorBundlePretty e @?= expectedErr ++ "\n"
        Right result -> assertFailure $ "Expected parse failure, but got: " ++ show result
