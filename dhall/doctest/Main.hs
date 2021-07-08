{-# LANGUAGE CPP #-}

module Main where

import System.FilePath ((</>))

import qualified GHC.IO.Encoding
import qualified System.Directory
import qualified System.Environment
import qualified System.IO
import qualified Test.DocTest
import qualified Test.Mockery.Directory

main :: IO ()
main = do

    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
    pwd    <- System.Directory.getCurrentDirectory
    prefix <- System.Directory.makeAbsolute pwd

    System.Environment.setEnv "XDG_CACHE_HOME" (pwd </> ".cache")

    Test.Mockery.Directory.inTempDirectory $ do
        writeFile "makeBools.dhall" "λ(n : Bool) → [ n && True, n && False, n || True, n || False ]"
        writeFile "bool1" "True"
        writeFile "bool2" "False"
        writeFile "both" "./bool1 && ./bool2"
        writeFile "file2" "./file1"
        writeFile "file1" "./file2"
        writeFile "simon.dhall" $ unlines
          [ "let Name = Text"
          , "let Font = < Arial | `Comic Sans` | Helvetica | `Times New Roman` >"
          , "let Person = { name : Name, favoriteFont : Font }"
          , "in  { name = \"Simon\", favoriteFont = Font.`Comic Sans` } : Person"
          ]

        Test.DocTest.doctest
            [ "-DWITH_HTTP"
            , "-DUSE_HTTP_CLIENT_TLS"
            , "--fast"
            , "--verbose"
            , prefix </> "ghc-src"

            -- Unfortunately we cannot target the entire @src@ directory.
            -- The reason is that src/Dhall/Version.hs depends on
            -- the generated Paths_dhall module which is "out-of-scope"
            -- when running the testsuite with cabal v1-test.
            -- Instead, we target a selection of modules whose combined module
            -- dependency tree covers all modules that contain doctests.

            -- , prefix </> "src"
            , "-i" <> (prefix </> "src")
#if __GLASGOW_HASKELL__ >= 806
            , prefix </> "src/Dhall/Deriving.hs"
#endif
            , prefix </> "src/Dhall/Tags.hs"
            , prefix </> "src/Dhall/Tutorial.hs"
            ]
