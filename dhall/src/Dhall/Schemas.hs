{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| This module contains the implementation of the @dhall rewrite-with-schemas@
    subcommand
-}

module Dhall.Schemas
    ( -- | Schemas
      schemasCommand
    , Schemas(..)
    , rewriteWithSchemas
    , SchemasError(..)
    ) where

import Control.Applicative (empty)
import Control.Exception   (Exception)
import Data.Maybe          (fromMaybe)
import Data.Text           (Text)
import Data.Void           (Void)
import Dhall.Crypto        (SHA256Digest)
import Dhall.Map           (Map)
import Dhall.Pretty        (CharacterSet (..), detectCharacterSet)
import Dhall.Src           (Src)
import Dhall.Syntax        (Expr (..), Import, Var (..))
import Dhall.Util
    ( Censor (..)
    , MultipleCheckFailed (..)
    , Header (..)
    , Input (..)
    , OutputMode (..)
    )

import qualified Control.Exception                         as Exception
import qualified Data.Map
import qualified Data.Maybe                                as Maybe
import qualified Data.Text.IO                              as Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Data.Void                                 as Void
import qualified Dhall.Core                                as Core
import qualified Dhall.Import                              as Import
import qualified Dhall.Map                                 as Map
import qualified Dhall.Normalize                           as Normalize
import qualified Dhall.Optics                              as Optics
import qualified Dhall.Parser                              as Parser
import qualified Dhall.Pretty
import qualified Dhall.Substitution                        as Substitution
import qualified Dhall.Syntax                              as Syntax
import qualified Dhall.TypeCheck                           as TypeCheck
import qualified Dhall.Util                                as Util
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite
import qualified System.Console.ANSI                       as ANSI
import qualified System.IO                                 as IO

-- | Arguments to the @rewrite-with-schemas@ subcommand
data Schemas = Schemas
    { chosenCharacterSet :: Maybe CharacterSet
    , commentControl     :: Util.WhitespaceControl
    , censor             :: Censor
    , input              :: Input
    , outputMode         :: OutputMode
    , schemas            :: Text
    }

-- | Implementation of the @dhall rewrite-with-schemas@ subcommand
schemasCommand :: Schemas -> IO ()
schemasCommand Schemas{..} = do
    (inputName, originalText) <- case input of
        InputFile file -> (,) file <$> Text.IO.readFile file
        StandardInput  -> (,) "(input)" <$> Text.IO.getContents

    (Header header, expression) <- Util.getExpressionAndHeaderFromStdinText commentControl censor inputName originalText

    let characterSet = fromMaybe (detectCharacterSet expression) chosenCharacterSet

    schemasRecord <- Core.throws (Parser.exprFromText "(schemas)" schemas)

    schemasExpression <- rewriteWithSchemas schemasRecord expression

    let docStream =
            Dhall.Pretty.layout
                (   Pretty.pretty header
                <>  Dhall.Pretty.prettyCharacterSet characterSet schemasExpression
                <>  "\n"
                )

    let schemasText = Pretty.Text.renderStrict docStream

    case outputMode of
        Write ->
            case input of
                InputFile file ->
                    if originalText == schemasText
                        then return ()
                        else AtomicWrite.atomicWriteFile
                                file
                                (Pretty.Text.renderLazy docStream)
                StandardInput -> do
                    supportsANSI <- ANSI.hSupportsANSI IO.stdout

                    Pretty.Terminal.renderIO
                        IO.stdout
                        (if supportsANSI
                            then fmap Dhall.Pretty.annToAnsiStyle docStream
                            else Pretty.unAnnotateS docStream)

        Check ->
            if originalText == schemasText
                then return ()
                else do
                    let command = "rewrite-with-schemas"

                    let modified = "rewritten"

                    let inputs = pure input

                    Exception.throwIO MultipleCheckFailed{..}

decodeSchema :: Expr s Void -> Maybe (Expr s Void, Map Text (Expr s Void))
decodeSchema (RecordLit m)
        | Just  _Type               <- Core.recordFieldValue <$> Map.lookup "Type" m
        , Just (RecordLit _default) <- Core.recordFieldValue <$> Map.lookup "default" m =
            Just (_Type, Core.recordFieldValue <$> _default)
decodeSchema _ =
    Nothing

decodeSchemas
    :: Expr s Void
    -> Maybe (Data.Map.Map SHA256Digest (Text, Map Text (Expr s Void)))
decodeSchemas (RecordLit keyValues) = do
    m <- traverse (decodeSchema . Core.recordFieldValue) keyValues

    let typeMetadata = Data.Map.fromList $ do
            (name, (_Type, _default)) <- Map.toList m

            return (Import.hashExpression (Syntax.denote _Type), (name, _default))

    return typeMetadata
decodeSchemas  _ =
    empty

-- | Simplify a Dhall expression using a record of schemas
rewriteWithSchemas
    :: Expr Src Import
    -- ^ Record of schemas
    -> Expr Src Import
    -- ^ Expression to simplify using the supplied schemas
    -> IO (Expr Src Import)
rewriteWithSchemas _schemas expression = do
    resolvedSchemas    <- Import.load _schemas
    resolvedExpression <- Import.load expression

    _ <- Core.throws (TypeCheck.typeOf resolvedSchemas)
    _ <- Core.throws (TypeCheck.typeOf resolvedExpression)

    let normalizedSchemas    = Normalize.normalize resolvedSchemas
    let normalizedExpression = Normalize.normalize resolvedExpression

    typeMetadata <- case decodeSchemas normalizedSchemas of
        Just typeMetadata -> return typeMetadata
        Nothing           -> Exception.throwIO NotASchemaRecord

    let schemasRewrite subExpression@(RecordLit keyValues) =
            Maybe.fromMaybe subExpression $ do
                let substitutions = Map.singleton "schemas" normalizedSchemas

                let substitutedExpression =
                        Substitution.substitute subExpression substitutions

                hash <- case TypeCheck.typeOf substitutedExpression of
                    Left _ ->
                        empty
                    Right subExpressionType ->
                        return (Import.hashExpression (Syntax.denote subExpressionType))

                (name, _default) <- Data.Map.lookup hash typeMetadata

                let diff a b | a == b    = Nothing
                             | otherwise = Just a

                let defaultedKeyValues =
                        Core.makeRecordField <$>
                        Map.fromMap (
                            Data.Map.differenceWith diff
                                (Map.toMap $ Core.recordFieldValue <$> keyValues)
                                (Map.toMap _default))

                let defaultedRecord = RecordLit defaultedKeyValues

                return (RecordCompletion (Field "schemas" $ Core.makeFieldSelection name) defaultedRecord)
        schemasRewrite subExpression =
            subExpression

    let rewrittenExpression :: Expr Src Import
        rewrittenExpression =
            fmap Void.absurd (Optics.transformOf Syntax.subExpressions schemasRewrite normalizedExpression)

    if Normalize.freeIn (V "schemas" 0) rewrittenExpression
        then return (Let (Syntax.makeBinding "schemas" _schemas) rewrittenExpression)
        else return expression

-- | Errors that can be thrown by `rewriteWithSchemas`
data SchemasError = NotASchemaRecord
    deriving (Exception)

instance Show SchemasError where
    show NotASchemaRecord =
        Util._ERROR <> ": The --schemas argument is not a record of schemas"
