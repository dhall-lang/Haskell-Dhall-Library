module Main where

import System.FilePath ((</>))
import Test.Tasty      (TestTree)

import qualified Dhall.Test.Dhall
import qualified Dhall.Test.Diff
import qualified Dhall.Test.Format
import qualified Dhall.Test.Freeze
import qualified Dhall.Test.Import
import qualified Dhall.Test.Lint
import qualified Dhall.Test.Normalization
import qualified Dhall.Test.Parser
import qualified Dhall.Test.QuickCheck
import qualified Dhall.Test.Regression
import qualified Dhall.Test.Schemas
import qualified Dhall.Test.SemanticHash
import qualified Dhall.Test.Tags
import qualified Dhall.Test.TH
import qualified Dhall.Test.Tutorial
import qualified Dhall.Test.TypeInference
import qualified GHC.IO.Encoding
import qualified System.Directory
import qualified System.Environment
import qualified System.IO
import qualified Test.Tasty

getAllTests :: IO TestTree
getAllTests = do
    normalizationTests <- Dhall.Test.Normalization.getTests

    parsingTests <- Dhall.Test.Parser.getTests

    formattingTests <- Dhall.Test.Format.getTests

    typeinferenceTests <- Dhall.Test.TypeInference.getTests

    importingTests <- Dhall.Test.Import.getTests

    lintTests <- Dhall.Test.Lint.getTests

    tagsTests <- Dhall.Test.Tags.getTests

    diffTests <- Dhall.Test.Diff.getTests

    semanticHashTests <- Dhall.Test.SemanticHash.getTests

    freezeTests <- Dhall.Test.Freeze.getTests

    schemaTests <- Dhall.Test.Schemas.getTests

    let testTree =
            Test.Tasty.testGroup "Dhall Tests"
                [ normalizationTests
                , parsingTests
                , importingTests
                , typeinferenceTests
                , formattingTests
                , lintTests
                , diffTests
                , semanticHashTests
                , tagsTests
                , freezeTests
                , schemaTests
                , Dhall.Test.Regression.tests
                , Dhall.Test.Tutorial.tests
                , Dhall.Test.QuickCheck.tests
                , Dhall.Test.Dhall.tests
                , Dhall.Test.TH.tests
                ]

    return testTree

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    pwd <- System.Directory.getCurrentDirectory

    System.Environment.setEnv "XDG_CACHE_HOME" (pwd </> ".cache")

    System.Environment.setEnv "DHALL_TEST_VAR" "6 * 7"

    -- Temporarily disabled to help with
    -- https://github.com/dhall-lang/dhall-haskell/issues/2237:
    --
    -- Make test failures easier to find by eliding the successes.
    -- https://github.com/feuerbach/tasty/issues/273#issuecomment-657054281
    -- System.Environment.setEnv "TASTY_HIDE_SUCCESSES" "true"

    allTests <- getAllTests

    Test.Tasty.defaultMain allTests
