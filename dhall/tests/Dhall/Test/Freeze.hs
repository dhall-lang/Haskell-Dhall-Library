{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Freeze where

import Data.Text    (Text)
import Dhall.Freeze (Intent (..), Scope (..))
import Prelude      hiding (FilePath)
import Test.Tasty   (TestTree)
import Turtle       (FilePath)

import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text.IO
import qualified Dhall.Core       as Core
import qualified Dhall.Freeze     as Freeze
import qualified Dhall.Parser     as Parser
import qualified Dhall.Test.Util  as Test.Util
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Turtle

freezeDirectory :: FilePath
freezeDirectory = "./tests/freeze"

getTests :: IO TestTree
getTests = do
    freezeTests <- Test.Util.discover (Turtle.chars <* "A.dhall") freezeTest (Turtle.lstree freezeDirectory)

    let testTree = Tasty.testGroup "freeze tests" [ freezeTests ]

    return testTree

freezeTest :: Text -> TestTree
freezeTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> "A.dhall")
        let outputFile = Text.unpack (prefix <> "B.dhall")

        inputText <- Text.IO.readFile inputFile

        parsedInput <- Core.throws (Parser.exprFromText mempty inputText)

        actualExpression <- Freeze.freezeExpression (Turtle.encodeString freezeDirectory) Parser.UnsupportedCommentsPermitted AllImports Cache parsedInput

        let actualText = Core.pretty actualExpression <> "\n"

        expectedText <- Text.IO.readFile outputFile

        let message = "The linted expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedText actualText
