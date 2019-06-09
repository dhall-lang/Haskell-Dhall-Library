{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Import where

import Control.Exception (catch)
import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.Import (MissingImports(..))
import Dhall.Parser (SourcedException(..))
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Core                       as Core
import qualified Dhall.Import                     as Import
import qualified Dhall.Parser                     as Parser
import qualified Dhall.Test.Util                  as Test.Util
import qualified System.FilePath                  as FilePath
import qualified Test.Tasty                       as Tasty
import qualified Test.Tasty.HUnit                 as Tasty.HUnit
import qualified Turtle

importDirectory :: FilePath
importDirectory = "./dhall-lang/tests/import"

getTests :: IO TestTree
getTests = do
    successTests <- Test.Util.discover (Turtle.chars <> "A.dhall") successTest (Turtle.lstree (importDirectory </> "success"))

    failureTests <- Test.Util.discover (Turtle.chars <> ".dhall") failureTest (Turtle.lstree (importDirectory </> "failure"))

    let testTree =
            Tasty.testGroup "import tests"
                [ successTests
                , failureTests
                ]

    return testTree

successTest :: Text -> TestTree
successTest path = do
    let pathString = Text.unpack path

    let directoryString = FilePath.takeDirectory pathString

    Tasty.HUnit.testCase pathString (do

        text <- Text.IO.readFile pathString

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        let open = Turtle.export "XDG_CACHE_HOME" "dhall-lang/tests/import/cache"

        let close _ = Turtle.unset "XDG_CACHE_HOME"

        if pathString == "./dhall-lang/tests/import/success/hashFromCacheA.dhall"
            then Turtle.managed (Exception.bracket open close)
            else return ()

        _ <- State.evalStateT (Import.loadWith actualExpr) (Import.emptyStatus directoryString)

        return () )

failureTest :: Text -> TestTree
failureTest path = do
    let pathString = Text.unpack path

    Tasty.HUnit.testCase pathString (do
        text <- Text.IO.readFile pathString

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        catch
          (do _ <- Import.load actualExpr

              fail "Import should have failed, but it succeeds")
          (\(SourcedException _ (MissingImports _)) -> pure ()) )
