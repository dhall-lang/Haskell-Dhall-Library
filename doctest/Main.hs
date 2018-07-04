module Main where

import Data.Monoid ((<>))
import System.FilePath ((</>))

import qualified System.Directory
import qualified System.IO.Temp
import qualified Test.DocTest

main :: IO ()
main = do
    pwd    <- System.Directory.getCurrentDirectory
    prefix <- System.Directory.makeAbsolute pwd

    System.IO.Temp.withSystemTempDirectory "doctest" $ \directory -> do
        System.Directory.setCurrentDirectory directory

        writeFile "makeBools" "λ(n : Bool) → [ n && True, n && False, n || True, n || False ]"
        writeFile "bool1" "True"
        writeFile "bool2" "False"
        writeFile "both" "./bool1 && ./bool2"
        writeFile "file2" "./file1"
        writeFile "file1" "./file2"

        Test.DocTest.doctest
            [ "--fast"
            , "-i" <> (prefix </> "src")
            , prefix </> "src/Dhall.hs"
            , prefix </> "src/Dhall/Import.hs"
            , prefix </> "src/Dhall/Tutorial.hs"
            ]
