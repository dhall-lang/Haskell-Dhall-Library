module Main where

import Control.Monad       (unless)
import Data.Text           (unpack)
import Data.Void           (Void)
import Dhall.Parser        (Src)
import Test.Tasty          (TestTree)
import Test.Tasty.HUnit    (HasCallStack, Assertion, assertFailure)
import Toml.Type.TOML      (TOML, tomlDiff)
import Toml.Type.Printer   (pretty)

import qualified Toml.Parser
import qualified Data.Text.IO
import qualified Dhall.Core      as Core
import qualified Dhall.Parser
import qualified Dhall.Import
import qualified Dhall.Toml
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Test.Tasty.defaultMain testTree


testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-toml"
        [ Test.Tasty.testGroup "dhall-to-toml" dhallToTomlTests
        , Test.Tasty.testGroup "toml-to-dhall" tomlToDhallTests
        ]
    where
        dhallToTomlTests = map testDhallToToml
            [ "./tasty/data/empty"
            , "./tasty/data/natural"
            , "./tasty/data/float"
            , "./tasty/data/multiple-fields"
            , "./tasty/data/nested-tables"
            , "./tasty/data/adjacent-tables"
            , "./tasty/data/inline-list"
            , "./tasty/data/record-list"
            , "./tasty/data/union-empty"
            , "./tasty/data/union-typed"
            ]
        tomlToDhallTests = map testTomlToDhall
            [ "./tasty/data/empty"
            , "./tasty/data/natural"
            , "./tasty/data/float"
            , "./tasty/data/multiple-fields"
            , "./tasty/data/nested-tables"
            , "./tasty/data/adjacent-tables"
            , "./tasty/data/inline-list"
            , "./tasty/data/record-list"
            , "./tasty/data/union-empty"
            , "./tasty/data/union-typed"
            ]

testDhallToToml :: String -> TestTree
testDhallToToml prefix = Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix ++ ".dhall"
    let outputFile = prefix ++ ".toml"
    text <- Data.Text.IO.readFile inputFile
    parsedExpression <-
        Core.throws (Dhall.Parser.exprFromText inputFile text)
    resolvedExpression <- Dhall.Import.load parsedExpression
    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)
    actualValue <-
        Core.throws (Dhall.Toml.dhallToToml resolvedExpression)
    inputText <- Data.Text.IO.readFile outputFile
    expectedValue <- case Toml.Parser.parse inputText of
        Left tomlErr -> fail $ show tomlErr
        Right expectedValue -> return expectedValue
    let message = "Conversion to TOML did not generate the expected output"
    assertTomlEq message expectedValue actualValue

fileToDhall :: String -> IO (Core.Expr Src Void)
fileToDhall file = do
    text <- Data.Text.IO.readFile file
    parsedExpression <-
        Core.throws (Dhall.Parser.exprFromText file text)
    resolvedExpression <- Dhall.Import.load parsedExpression
    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)
    return resolvedExpression

testTomlToDhall :: String -> TestTree
testTomlToDhall prefix = Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix ++ ".toml"
    let schemaFile = prefix ++ "-schema.dhall"
    let outputFile = prefix ++ ".dhall"
    inputText <- Data.Text.IO.readFile inputFile
    toml <- case Toml.Parser.parse inputText of
        Left tomlErr -> fail $ show tomlErr
        Right toml -> return toml
    schema <- fileToDhall schemaFile
    actualValue <- case Dhall.Toml.tomlToDhall schema toml of
        Left err -> fail $ show err
        Right val -> return val
    expectedValue <- fileToDhall outputFile
    let message = "Conversion to Dhall did not generate the expected output"
    assertDhallEq message (Core.normalize expectedValue) actualValue


assertTomlEq :: HasCallStack => String -> TOML -> TOML -> Assertion
assertTomlEq prefix expected actual  = unless (expected == actual) (assertFailure msg)
    where
        pretty' = unpack . pretty
        msg = prefix ++ "\nExpected:\n" ++ pretty' expected ++ "\nActual:\n" ++ pretty' actual ++
            "Diff:\nMissing:\n" ++ pretty' (tomlDiff expected actual) ++
            "\nExtra:\n" ++ pretty' (tomlDiff actual expected) ++
            "AST:\nExpected:\n" ++ show expected ++ "\nActual:\n" ++ show actual

assertDhallEq :: HasCallStack => String -> Core.Expr Src Void -> Core.Expr Src Void -> Assertion
assertDhallEq prefix expected actual = unless (expected == actual) (assertFailure msg)
    where
        pretty' = unpack . Core.pretty
        msg = prefix ++ "\nExpected:\n" ++ pretty' expected ++ "\nActual:\n" ++ pretty' actual ++
            "AST:\nExpected:\n" ++ show expected ++  "\nActual:\n" ++ show actual


