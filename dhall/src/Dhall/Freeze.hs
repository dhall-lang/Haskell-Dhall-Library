{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module contains the implementation of the @dhall freeze@ subcommand

module Dhall.Freeze
    ( -- * Freeze
      freeze
    , freezeWithManager
    , freezeExpression
    , freezeExpressionWithManager
    , freezeImport
    , freezeImportWithManager
    , freezeRemoteImport
    , freezeRemoteImportWithManager

      -- * Types
    , Scope(..)
    , Intent(..)
    ) where

import Data.Foldable       (for_)
import Data.List.NonEmpty  (NonEmpty)
import Data.Maybe          (fromMaybe)
import Dhall.Pretty        (CharacterSet, detectCharacterSet)
import Dhall.Syntax
    ( Expr (..)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    )
import Dhall.Util
    ( Censor
    , CheckFailed (..)
    , Header (..)
    , Input (..)
    , OutputMode (..)
    , Transitivity (..)
    , handleMultipleChecksFailed
    )
import System.Console.ANSI (hSupportsANSI)

import qualified Control.Exception                         as Exception
import qualified Control.Monad.Trans.State.Strict          as State
import qualified Data.Text.IO                              as Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Dhall.Core                                as Core
import qualified Dhall.Import
import qualified Dhall.Optics
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Dhall.Util                                as Util
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite.LazyText
import qualified System.FilePath
import qualified System.IO

-- | Retrieve an `Import` and update the hash to match the latest contents
freezeImport
    :: FilePath
    -- ^ Current working directory
    -> Util.WhitespaceControl
    -> Import
    -> IO Import
freezeImport = freezeImportWithManager Dhall.Import.defaultNewManager

-- | See 'freezeImport'.
freezeImportWithManager
    :: IO Dhall.Import.Manager
    -> FilePath
    -> Util.WhitespaceControl
    -> Import
    -> IO Import
freezeImportWithManager newManager directory commentControl import_ = do
    let unprotectedImport =
            import_
                { importHashed =
                    (importHashed import_)
                        { hash = Nothing
                        }
                }

    let status = (Dhall.Import.emptyStatusWithManager newManager directory)
            { Dhall.Import._commentControl = commentControl }

    expression <- State.evalStateT (Dhall.Import.loadWith (Embed unprotectedImport)) status

    case Dhall.TypeCheck.typeOf expression of
        Left  exception -> Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression = Core.alphaNormalize (Core.normalize expression)

    -- make sure the frozen import is present in the semantic cache
    Dhall.Import.writeExpressionToSemanticCache (Core.denote expression)

    let expressionHash = Dhall.Import.hashExpression normalizedExpression

    let newImportHashed = (importHashed import_) { hash = Just expressionHash }

    let newImport = import_ { importHashed = newImportHashed }

    return newImport

-- | Freeze an import only if the import is a `Remote` import
freezeRemoteImport
    :: FilePath
    -- ^ Current working directory
    -> Util.WhitespaceControl
    -> Import
    -> IO Import
freezeRemoteImport = freezeRemoteImportWithManager Dhall.Import.defaultNewManager

-- | See 'freezeRemoteImport'.
freezeRemoteImportWithManager
    :: IO Dhall.Import.Manager
    -> FilePath
    -> Util.WhitespaceControl
    -> Import
    -> IO Import
freezeRemoteImportWithManager newManager directory commentControl import_ =
    case importType (importHashed import_) of
        Remote {} -> freezeImportWithManager newManager directory commentControl import_
        _         -> return import_

-- | Specifies which imports to freeze
data Scope
    = OnlyRemoteImports
    -- ^ Freeze only remote imports (i.e. URLs)
    | AllImports
    -- ^ Freeze all imports (including paths and environment variables)

-- | Specifies why we are adding semantic integrity checks
data Intent
    = Secure
    -- ^ Protect imports with an integrity check without a fallback so that
    --   import resolution fails if the import changes
    | Cache
    -- ^ Protect imports with an integrity check and also add a fallback import
    --   import without an integrity check.  This is useful if you only want to
    --   cache imports when possible but still gracefully degrade to resolving
    --   them if the semantic integrity check has changed.

-- | Implementation of the @dhall freeze@ subcommand
freeze
    :: OutputMode
    -> Transitivity
    -> NonEmpty Input
    -> Scope
    -> Intent
    -> Maybe CharacterSet
    -> Util.WhitespaceControl
    -> Censor
    -> IO ()
freeze = freezeWithManager Dhall.Import.defaultNewManager

-- | See 'freeze'.
freezeWithManager
    :: IO Dhall.Import.Manager
    -> OutputMode
    -> Transitivity
    -> NonEmpty Input
    -> Scope
    -> Intent
    -> Maybe CharacterSet
    -> Util.WhitespaceControl
    -> Censor
    -> IO ()
freezeWithManager newManager outputMode transitivity0 inputs scope intent chosenCharacterSet commentControl censor =
    handleMultipleChecksFailed "freeze" "frozen" go inputs
  where
    go input = do
        let directory = case input of
                StandardInput ->
                    "."
                InputFile file ->
                    System.FilePath.takeDirectory file

        let status = (Dhall.Import.emptyStatusWithManager newManager directory)
                { Dhall.Import._commentControl = commentControl }

        (inputName, originalText, transitivity) <- case input of
            InputFile file -> do
                text <- Text.IO.readFile file

                return (file, text, transitivity0)

            StandardInput -> do
                text <- Text.IO.getContents

                return ("(input)", text, NonTransitive)

        (Header header, parsedExpression) <- Util.getExpressionAndHeaderFromStdinText commentControl censor inputName originalText

        let characterSet = fromMaybe (detectCharacterSet parsedExpression) chosenCharacterSet

        case transitivity of
            Transitive ->
                for_ parsedExpression $ \import_ -> do
                    maybeFilepath <- Dhall.Import.dependencyToFile status import_

                    for_ maybeFilepath $ \filepath ->
                        go (InputFile filepath)

            NonTransitive ->
                return ()

        frozenExpression <- freezeExpressionWithManager newManager directory commentControl scope intent parsedExpression

        let doc =  Pretty.pretty header
                <> Dhall.Pretty.prettyCharacterSet characterSet frozenExpression
                <> "\n"

        let stream = Dhall.Pretty.layout doc

        let modifiedText = Pretty.Text.renderStrict stream

        case outputMode of
            Write -> do
                let unAnnotated = Pretty.unAnnotateS stream

                case input of
                    InputFile file ->
                        if originalText == modifiedText
                            then return ()
                            else
                                AtomicWrite.LazyText.atomicWriteFile
                                    file
                                    (Pretty.Text.renderLazy unAnnotated)

                    StandardInput -> do
                        supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
                        if supportsANSI
                           then
                             Pretty.renderIO System.IO.stdout (Dhall.Pretty.annToAnsiStyle <$> stream)
                           else
                             Pretty.renderIO System.IO.stdout unAnnotated

                return (Right ())

            Check ->
                return $
                    if originalText == modifiedText
                        then Right ()
                        else Left CheckFailed{..}

{-| Slightly more pure version of the `freeze` function

    This still requires `IO` to freeze the import, but now the input and output
    expression are passed in explicitly
-}
freezeExpression
    :: FilePath
    -- ^ Starting directory
    -> Util.WhitespaceControl
    -> Scope
    -> Intent
    -> Expr s Import
    -> IO (Expr s Import)
freezeExpression = freezeExpressionWithManager Dhall.Import.defaultNewManager

-- | See 'freezeExpression'.
freezeExpressionWithManager
    :: IO Dhall.Import.Manager
    -> FilePath
    -> Util.WhitespaceControl
    -> Scope
    -> Intent
    -> Expr s Import
    -> IO (Expr s Import)
freezeExpressionWithManager newManager directory commentControl scope intent expression = do
    let freezeScope =
            case scope of
                AllImports        -> freezeImportWithManager
                OnlyRemoteImports -> freezeRemoteImportWithManager

    let freezeFunction = freezeScope newManager directory commentControl

    let cache
            -- This case is necessary because `transformOf` is a bottom-up
            -- rewrite rule.   Without this rule, if you were to transform a
            -- file that already has a cached expression, like this:
            --
            --     someImport sha256:… ? someImport
            --
            -- ... then you would get:
            --
            --       (someImport sha256:… ? someImport)
            --     ? (someImport sha256:… ? someImport)
            --
            -- ... and this rule fixes that by collapsing that back to:
            --
            --       (someImport sha256:… ? someImport)
            (ImportAlt
                (Core.shallowDenote -> ImportAlt
                    (Core.shallowDenote -> Embed
                        Import{ importHashed = ImportHashed{ hash = Just _expectedHash } }
                    )
                    (Core.shallowDenote -> Embed
                        Import{ importHashed = ImportHashed{ hash = Nothing } }
                    )
                )
                import_@(Core.shallowDenote -> ImportAlt
                    (Core.shallowDenote -> Embed
                        Import{ importHashed = ImportHashed{ hash = Just _actualHash } }
                    )
                    (Core.shallowDenote -> Embed
                        Import{ importHashed = ImportHashed{ hash = Nothing } }
                    )
                )
            ) =
                {- Here we could actually compare the `_expectedHash` and
                   `_actualHash` to see if they differ, but we choose not to do
                   so and instead automatically accept the `_actualHash`.  This
                   is done for the same reason that the `freeze*` functions
                   ignore hash mismatches: the user intention when using `dhall
                   freeze` is to update the hash, which they expect to possibly
                   change.
                -}
                return import_
        cache
            (Embed import_@(Import { importHashed = ImportHashed { hash = Nothing } })) = do
                frozenImport <- freezeFunction import_

                {- The two imports can be the same if the import is local and
                   `freezeFunction` only freezes remote imports by default
                -}
                if frozenImport /= import_
                    then return (ImportAlt (Embed frozenImport) (Embed import_))
                    else return (Embed import_)
        cache
            (Embed import_@(Import { importHashed = ImportHashed { hash = Just _ } })) = do
                -- Regenerate the integrity check, just in case it's wrong
                frozenImport <- freezeFunction import_

                -- `dhall freeze --cache` also works the other way around, adding an
                -- unprotected fallback import to imports that are already
                -- protected
                let thawedImport = import_
                        { importHashed = (importHashed import_)
                            { hash = Nothing
                            }
                        }

                return (ImportAlt (Embed frozenImport) (Embed thawedImport))
        cache expression_ =
            return expression_

    case intent of
        Secure ->
            traverse freezeFunction expression
        Cache  ->
            Dhall.Optics.transformMOf Core.subExpressions cache expression
