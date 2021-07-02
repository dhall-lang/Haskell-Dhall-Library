{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Format where

import Data.Text    (Text)
import Dhall.Parser (Header (..))
import Dhall.Pretty (CharacterSet (..))
import Test.Tasty   (TestTree)

import qualified Control.Monad                         as Monad
import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text.IO
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc.Render.Text
import qualified Dhall.Core                            as Core
import qualified Dhall.Format
import qualified Dhall.Parser                          as Parser
import qualified Dhall.Test.Util                       as Test.Util
import qualified Test.Tasty                            as Tasty
import qualified Test.Tasty.Silver                     as Tasty.Silver
import qualified Turtle

getTests :: IO TestTree
getTests = do
    let unicodeFiles = do
            path <- Turtle.lstree "./tests/format"

            let skip = [ "./tests/format/asciiA.dhall" ]

            Monad.guard (path `notElem` skip)

            return path

    unicodeTests <- Test.Util.discover (Turtle.chars <* "A.dhall") (formatTest Unicode) unicodeFiles

    asciiTests <- Test.Util.discover (Turtle.chars <* "A.dhall") (formatTest ASCII) (pure "./tests/format/asciiA.dhall")

    let testTree =
            Tasty.testGroup "format tests"
                [ unicodeTests
                , asciiTests
                ]

    return testTree

format :: CharacterSet -> (Header, Core.Expr Parser.Src Core.Import) -> Text
format characterSet (header, expr) =
    let docStream = Dhall.Format.formatHeaderAndExpr characterSet header expr
    in
        Doc.Render.Text.renderStrict docStream

formatTest :: CharacterSet -> Text -> TestTree
formatTest characterSet prefix =
    let inputFile  = Text.unpack (prefix <> "A.dhall")
        outputFile = Text.unpack (prefix <> "B.dhall")

        action = do
            inputText <- Text.IO.readFile inputFile

            headerAndExpr <- Core.throws (Parser.exprAndHeaderFromText Parser.UnsupportedCommentsPermitted mempty inputText)

            return (format characterSet headerAndExpr)
    in
        Tasty.Silver.goldenVsAction (Text.unpack prefix) outputFile action id
