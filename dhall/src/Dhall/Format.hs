{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @dhall format@ subcommand

module Dhall.Format
    ( -- * Format
      Format(..)
    , format
    , formatHeaderAndExpr
    ) where

import Data.Foldable      (for_)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe         (fromMaybe)
import Dhall.Core         (Expr, Import, internalError)
import Dhall.Pretty
    ( CharacterSet
    , annToAnsiStyle
    , detectCharacterSet
    )
import Dhall.Src          (Src)
import Dhall.Util
    ( Censor
    , CheckFailed (..)
    , Header (..)
    , Input (..)
    , OutputMode (..)
    , Transitivity (..)
    , handleMultipleChecksFailed
    )

import qualified Data.Text                                 as Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.Parser.Expression
import qualified Dhall.Pretty
import qualified Dhall.Pretty.Internal
import qualified Dhall.Util
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite.LazyText
import qualified System.Console.ANSI
import qualified System.FilePath
import qualified System.IO
import qualified Text.Megaparsec.Error                     as Megaparsec

-- | Arguments to the `format` subcommand
data Format = Format
    { chosenCharacterSet :: Maybe CharacterSet
    , commentControl     :: Dhall.Util.WhitespaceControl
    , censor             :: Censor
    , transitivity       :: Transitivity
    , inputs             :: NonEmpty Input
    , outputMode         :: OutputMode
    }

-- | Implementation of the @dhall format@ subcommand
format :: Format -> IO ()
format (Format { inputs = inputs0, transitivity = transitivity0, ..}) =
    handleMultipleChecksFailed "format" "formatted" go inputs0
  where
    go input = do
        let directory = case input of
                StandardInput ->
                    "."
                InputFile file ->
                    System.FilePath.takeDirectory file

        let status = (Dhall.Import.emptyStatus directory)
                { Dhall.Import._commentControl = commentControl }

        let layoutHeaderAndExpr (header, expr) =
                let characterSet = fromMaybe (detectCharacterSet expr) chosenCharacterSet
                in formatHeaderAndExpr characterSet header expr

        (inputName, originalText, transitivity) <- case input of
            InputFile file -> do
                text <- Data.Text.IO.readFile file

                return (file, text, transitivity0)
            StandardInput -> do
                text <- Data.Text.IO.getContents

                return ("(input)", text, NonTransitive)

        headerAndExpr@(_, parsedExpression) <- Dhall.Util.getExpressionAndHeaderFromStdinText commentControl censor inputName originalText

        case transitivity of
            Transitive ->
                for_ parsedExpression $ \import_ -> do
                    maybeFilepath <- Dhall.Import.dependencyToFile status import_

                    for_ maybeFilepath $ \filepath ->
                        go (InputFile filepath)

            NonTransitive ->
                return ()

        let docStream = layoutHeaderAndExpr headerAndExpr

        let formattedText = Pretty.Text.renderStrict docStream

        case outputMode of
            Write -> do
                case input of
                    InputFile file ->
                        if originalText == formattedText
                            then return ()
                            else AtomicWrite.LazyText.atomicWriteFile
                                    file
                                    (Pretty.Text.renderLazy docStream)

                    StandardInput -> do
                        supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                        Pretty.Terminal.renderIO
                            System.IO.stdout
                            (if supportsANSI
                                then (fmap annToAnsiStyle docStream)
                                else (Pretty.unAnnotateS docStream))

                return (Right ())

            Check ->
                return $
                    if originalText == formattedText
                        then Right ()
                        else Left CheckFailed{..}

-- | Format 'Header' and 'Expr' with a given 'CharacterSet'
formatHeaderAndExpr
    :: CharacterSet
    -> Header
    -> Expr Src Import
    -> Pretty.SimpleDocStream Dhall.Pretty.Ann
formatHeaderAndExpr characterSet (Header "") expr =
    Dhall.Pretty.layout
        (   Dhall.Pretty.prettyCharacterSet characterSet expr
        <>  "\n")
formatHeaderAndExpr characterSet (Header headerTxt) expr =
    case Dhall.Parser.runParser Dhall.Parser.Expression.multiComment Dhall.Util.UnsupportedCommentsForbidden "(header)" headerTxt of
        Right header ->
            Dhall.Pretty.layout
                (   Dhall.Pretty.Internal.renderComment False header
                <>  Pretty.hardline
                <>  Dhall.Pretty.prettyCharacterSet characterSet expr
                <>  "\n")
        Left e -> internalError $ "Dhall.Format.formatHeaderAndExpr: Invalid Header\n" <>
            Text.pack (Megaparsec.errorBundlePretty e)
