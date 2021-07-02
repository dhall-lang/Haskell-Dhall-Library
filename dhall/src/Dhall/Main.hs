{-| This module contains the top-level entrypoint and options parsing for the
    @dhall@ executable
-}

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Main
    ( -- * Options
      Options(..)
    , Mode(..)
    , ResolveMode(..)
    , parseOptions
    , parserInfoOptions

      -- * Execution
    , Dhall.Main.command
    , main
    ) where

import Control.Applicative       (optional, (<|>))
import Control.Exception         (Handler (..), SomeException)
import Control.Monad             (when)
import Data.Foldable             (for_)
import Data.Maybe                (fromMaybe)
import Data.List.NonEmpty        (NonEmpty (..), nonEmpty)
import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Data.Void                 (Void)
import Dhall.Freeze              (Intent (..), Scope (..))
import Dhall.Import
    ( Depends (..)
    , Imported (..)
    , SemanticCacheMode (..)
    , _semanticCacheMode
    )
import Dhall.Parser              (Src)
import Dhall.Pretty              (Ann, CharacterSet (..), annToAnsiStyle, detectCharacterSet)
import Dhall.Schemas             (Schemas (..))
import Dhall.TypeCheck
    ( Censored (..)
    , DetailedTypeError (..)
    , TypeError
    )
import Dhall.Version             (dhallVersionString)
import Options.Applicative       (Parser, ParserInfo)
import System.Exit               (ExitCode, exitFailure)
import System.IO                 (Handle)
import Text.Dot                  ((.->.))

import Dhall.Core
    ( Expr (Annot)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    , URL (..)
    , pretty
    )
import Dhall.Util
    ( Censor (..)
    , CheckFailed (..)
    , WhitespaceControl (..)
    , Header (..)
    , Input (..)
    , Output (..)
    , OutputMode (..)
    , Transitivity (..)
    , handleMultipleChecksFailed
    )

import qualified Codec.CBOR.JSON
import qualified Codec.CBOR.Read
import qualified Codec.CBOR.Write
import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict          as State
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Map
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Dhall
import qualified Dhall.Binary
import qualified Dhall.Core
import qualified Dhall.Diff
import qualified Dhall.DirectoryTree                       as DirectoryTree
import qualified Dhall.Format
import qualified Dhall.Freeze
import qualified Dhall.Import
import qualified Dhall.Import.Types
import qualified Dhall.Lint
import qualified Dhall.Map
import qualified Dhall.Pretty
import qualified Dhall.Repl
import qualified Dhall.Schemas
import qualified Dhall.Tags
import qualified Dhall.TypeCheck
import qualified Dhall.Util
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite.LazyText
import qualified System.Console.ANSI
import qualified System.Exit                               as Exit
import qualified System.FilePath
import qualified System.IO
import qualified Text.Dot
import qualified Text.Pretty.Simple

-- | Top-level program options
data Options = Options
    { mode               :: Mode
    , explain            :: Bool
    , plain              :: Bool
    , chosenCharacterSet :: Maybe CharacterSet
    , commentControl     :: WhitespaceControl
    , censor             :: Censor
    }

-- | The subcommands for the @dhall@ executable
data Mode
    = Default
          { file :: Input
          , output :: Output
          , annotate :: Bool
          , alpha :: Bool
          , semanticCacheMode :: SemanticCacheMode
          , version :: Bool
          }
    | Version
    | Resolve
          { file :: Input
          , resolveMode :: Maybe ResolveMode
          , semanticCacheMode :: SemanticCacheMode
          }
    | Type
          { file :: Input
          , quiet :: Bool
          , semanticCacheMode :: SemanticCacheMode
          }
    | Normalize { file :: Input , alpha :: Bool }
    | Repl
    | Format { deprecatedInPlace :: Bool, transitivity :: Transitivity, outputMode :: OutputMode, inputs :: NonEmpty Input }
    | Freeze { deprecatedInPlace :: Bool, transitivity :: Transitivity, all_ :: Bool, cache :: Bool, outputMode :: OutputMode, inputs :: NonEmpty Input }
    | Hash { file :: Input, cache :: Bool }
    | Diff { expr1 :: Text, expr2 :: Text }
    | Lint { deprecatedInPlace :: Bool, transitivity :: Transitivity, outputMode :: OutputMode, inputs :: NonEmpty Input }
    | Tags
          { input :: Input
          , output :: Output
          , suffixes :: Maybe [Text]
          , followSymlinks :: Bool
          }
    | Encode { file :: Input, json :: Bool }
    | Decode { file :: Input, json :: Bool, quiet :: Bool }
    | Text { file :: Input, output :: Output }
    | DirectoryTree { file :: Input, path :: FilePath }
    | Schemas { file :: Input, outputMode :: OutputMode, schemas :: Text }
    | SyntaxTree { file :: Input, noted :: Bool }

-- | This specifies how to resolve transitive dependencies
data ResolveMode
    = Dot
    -- ^ Generate a DOT file for @graphviz@
    | ListTransitiveDependencies
    -- ^ List all transitive dependencies as text, one per line
    | ListImmediateDependencies
    -- ^ List immediate dependencies as text, one per line

-- | Groups of subcommands
data Group
    = Manipulate
    | Generate
    | Interpret
    | Convert
    | Miscellaneous
    | Debugging

groupDescription :: Group -> String
groupDescription group = case group of
    Manipulate -> "Manipulate Dhall code"
    Generate -> "Generate other formats from Dhall"
    Interpret -> "Interpret Dhall"
    Convert -> "Convert Dhall to and from its binary representation"
    Miscellaneous -> "Miscellaneous"
    Debugging -> "Debugging this interpreter"

-- | `Parser` for the `Options` type
parseOptions :: Parser Options
parseOptions =
        Options
    <$> parseMode
    <*> switch "explain" "Explain error messages in more detail"
    <*> switch "plain" "Disable syntax highlighting"
    <*> parseCharacterSet
    <*> parseWhitespaceControl
    <*> parseCensor
  where
    switch name description =
        Options.Applicative.switch
            (   Options.Applicative.long name
            <>  Options.Applicative.help description
            )

    parseWhitespaceControl = fmap f $ switch
        "discard-unsupported-comments"
        "Discard comments that cannot be auto-formatted instead of failing (but continue to submit issues for any comments the auto-formatter rejects)"
      where
        f True  = UnsupportedCommentsPermitted
        f False = UnsupportedCommentsForbidden

    parseCensor = fmap f (switch "censor" "Hide source code in error messages")
      where
        f True  = Censor
        f False = NoCensor

    parseCharacterSet =
            Options.Applicative.flag'
                (Just Unicode)
                (   Options.Applicative.long "unicode"
                <>  Options.Applicative.help "Format code using only Unicode syntax"
                )
        <|> Options.Applicative.flag'
                (Just ASCII)
                (   Options.Applicative.long "ascii"
                <>  Options.Applicative.help "Format code using only ASCII syntax"
                )
        <|> pure Nothing

subcommand :: Group -> String -> String -> Parser a -> Parser a
subcommand group name description parser =
    Options.Applicative.hsubparser
        (   Options.Applicative.command name parserInfo
        <>  Options.Applicative.metavar name
        <>  Options.Applicative.commandGroup (groupDescription group)
        )
  where
    parserInfo =
        Options.Applicative.info parser
            (   Options.Applicative.fullDesc
            <>  Options.Applicative.progDesc description
            )

parseMode :: Parser Mode
parseMode =
        subcommand
            Manipulate
            "format"
            "Standard code formatter for the Dhall language"
            (Format <$> deprecatedInPlace <*> parseTransitiveSwitch <*> parseCheck "formatted" <*> parseFiles)
    <|> subcommand
            Manipulate
            "freeze"
            "Add integrity checks to remote import statements of an expression"
            (Freeze <$> deprecatedInPlace <*> parseTransitiveSwitch <*> parseAllFlag <*> parseCacheFlag <*> parseCheck "frozen" <*> parseFiles)
    <|> subcommand
            Manipulate
            "lint"
            "Improve Dhall code by using newer language features and removing dead code"
            (Lint <$> deprecatedInPlace <*> parseTransitiveSwitch <*> parseCheck "linted" <*> parseFiles)
    <|> subcommand
            Manipulate
            "rewrite-with-schemas"
            "Simplify Dhall code using a schemas record"
            (Dhall.Main.Schemas <$> parseInplaceNonTransitive <*> parseCheck "rewritten" <*> parseSchemasRecord)
    <|> subcommand
            Generate
            "text"
            "Render a Dhall expression that evaluates to a Text literal"
            (Text <$> parseFile <*> parseOutput)
    <|> subcommand
            Generate
            "to-directory-tree"
            "Convert nested records of Text literals into a directory tree"
            (DirectoryTree <$> parseFile <*> parseDirectoryTreeOutput)
    <|> subcommand
            Interpret
            "resolve"
            "Resolve an expression's imports"
            (Resolve <$> parseFile <*> parseResolveMode <*> parseSemanticCacheMode)
    <|> subcommand
            Interpret
            "type"
            "Infer an expression's type"
            (Type <$> parseFile <*> parseQuiet <*> parseSemanticCacheMode)
    <|> subcommand
            Interpret
            "normalize"
            "Normalize an expression"
            (Normalize <$> parseFile <*> parseAlpha)
    <|> subcommand
            Convert
            "encode"
            "Encode a Dhall expression to binary"
            (Encode <$> parseFile <*> parseJSONFlag)
    <|> subcommand
            Convert
            "decode"
            "Decode a Dhall expression from binary"
            (Decode <$> parseFile <*> parseJSONFlag <*> parseQuiet)
    <|> subcommand
            Miscellaneous
            "repl"
            "Interpret expressions in a REPL"
            (pure Repl)
    <|> subcommand
            Miscellaneous
            "diff"
            "Render the difference between the normal form of two expressions"
            (Diff <$> argument "expr1" <*> argument "expr2")
    <|> subcommand
            Miscellaneous
            "hash"
            "Compute semantic hashes for Dhall expressions"
            (Hash <$> parseFile <*> parseCache)
    <|> subcommand
            Miscellaneous
            "tags"
            "Generate etags file"
            (Tags <$> parseInput <*> parseTagsOutput <*> parseSuffixes <*> parseFollowSymlinks)
    <|> subcommand
            Miscellaneous
            "version"
            "Display version"
            (pure Version)
    <|> subcommand
            Debugging
            "haskell-syntax-tree"
            "Output the parsed syntax tree (for debugging)"
            (SyntaxTree <$> parseFile <*> parseNoted)
    <|> (   Default
        <$> parseFile
        <*> parseOutput
        <*> parseAnnotate
        <*> parseAlpha
        <*> parseSemanticCacheMode
        <*> parseVersion
        )
  where
    deprecatedInPlace =
        Options.Applicative.switch
            (   Options.Applicative.long "inplace"
            <>  Options.Applicative.internal -- completely hidden from help
            )

    argument =
            fmap Data.Text.pack
        .   Options.Applicative.strArgument
        .   Options.Applicative.metavar

    parseFile = fmap f (optional p)
      where
        f  Nothing    = StandardInput
        f (Just file) = InputFile file

        p = Options.Applicative.strOption
                (   Options.Applicative.long "file"
                <>  Options.Applicative.help "Read expression from a file instead of standard input"
                <>  Options.Applicative.metavar "FILE"
                <>  Options.Applicative.action "file"
                )

    parseFiles = fmap f (Options.Applicative.many p)
      where
        -- Parse explicit stdin in the input filepaths
        parseStdin inputs
            | any (== InputFile "-") inputs = StandardInput : filter (/= InputFile "-") inputs
            | otherwise = inputs

        f = fromMaybe (pure StandardInput) . nonEmpty . parseStdin . fmap InputFile

        p = Options.Applicative.strArgument
                (   Options.Applicative.help "Read expression from files instead of standard input"
                <>  Options.Applicative.metavar "FILES"
                <>  Options.Applicative.action "file"
                )

    parseOutput = fmap f (optional p)
      where
        f Nothing = StandardOutput
        f (Just file) = OutputFile file

        p = Options.Applicative.strOption
                (   Options.Applicative.long "output"
                <>  Options.Applicative.help "Write result to a file instead of standard output"
                <>  Options.Applicative.metavar "FILE"
                <>  Options.Applicative.action "file"
                )

    parseAlpha =
        Options.Applicative.switch
            (   Options.Applicative.long "alpha"
            <>  Options.Applicative.help "α-normalize expression"
            )

    parseAnnotate =
        Options.Applicative.switch
            (   Options.Applicative.long "annotate"
            <>  Options.Applicative.help "Add a type annotation to the output"
            )

    parseSemanticCacheMode =
        Options.Applicative.flag
            UseSemanticCache
            IgnoreSemanticCache
            (   Options.Applicative.long "no-cache"
            <>  Options.Applicative.help
                  "Handle protected imports as if the cache was empty"
            )

    parseVersion =
        Options.Applicative.switch
            (   Options.Applicative.long "version"
            <>  Options.Applicative.help "Display version"
            )

    parseResolveMode =
          Options.Applicative.flag' (Just Dot)
              (   Options.Applicative.long "dot"
              <>  Options.Applicative.help
                    "Output import dependency graph in dot format"
              )
        <|>
          Options.Applicative.flag' (Just ListImmediateDependencies)
              (   Options.Applicative.long "immediate-dependencies"
              <>  Options.Applicative.help
                    "List immediate import dependencies"
              )
        <|>
          Options.Applicative.flag' (Just ListTransitiveDependencies)
              (   Options.Applicative.long "transitive-dependencies"
              <>  Options.Applicative.help
                    "List transitive import dependencies in post-order"
              )
        <|> pure Nothing

    parseQuiet =
        Options.Applicative.switch
            (   Options.Applicative.long "quiet"
            <>  Options.Applicative.help "Don't print the result"
            )

    parseInplace =
        Options.Applicative.strOption
            (   Options.Applicative.long "inplace"
            <>  Options.Applicative.help "Modify the specified file in-place"
            <>  Options.Applicative.metavar "FILE"
            <>  Options.Applicative.action "file"
            )

    parseTransitiveSwitch = Options.Applicative.flag NonTransitive Transitive
        (   Options.Applicative.long "transitive"
        <>  Options.Applicative.help "Modify the input and its transitive relative imports in-place"
        )

    parseInplaceNonTransitive =
            fmap InputFile parseInplace
        <|> pure StandardInput

    parseInput = fmap f (optional p)
      where
        f  Nothing    = StandardInput
        f (Just path) = InputFile path

        p = Options.Applicative.strOption
            (   Options.Applicative.long "path"
            <>  Options.Applicative.help "Index all files in path recursively. Will get list of files from STDIN if omitted."
            <>  Options.Applicative.metavar "PATH"
            <>  Options.Applicative.action "file"
            <>  Options.Applicative.action "directory"
            )

    parseTagsOutput = fmap f (optional p)
      where
        f  Nothing    = OutputFile "tags"
        f (Just file) = OutputFile file

        p = Options.Applicative.strOption
            (   Options.Applicative.long "output"
            <>  Options.Applicative.help "The name of the file that the tags are written to. Defaults to \"tags\""
            <>  Options.Applicative.metavar "FILENAME"
            <>  Options.Applicative.action "file"
            )

    parseSuffixes = fmap f (optional p)
      where
        f  Nothing    = Just [".dhall"]
        f (Just "")   = Nothing
        f (Just line) = Just (Data.Text.splitOn " " line)

        p = Options.Applicative.strOption
            (   Options.Applicative.long "suffixes"
            <>  Options.Applicative.help "Index only files with suffixes. \"\" to index all files."
            <>  Options.Applicative.metavar "SUFFIXES"
            )

    parseFollowSymlinks =
        Options.Applicative.switch
        (   Options.Applicative.long "follow-symlinks"
        <>  Options.Applicative.help "Follow symlinks when recursing directories"
        )

    parseJSONFlag =
        Options.Applicative.switch
        (   Options.Applicative.long "json"
        <>  Options.Applicative.help "Use JSON representation of CBOR"
        )

    parseAllFlag =
        Options.Applicative.switch
        (   Options.Applicative.long "all"
        <>  Options.Applicative.help "Add integrity checks to all imports (not just remote imports)"
        )

    parseCacheFlag =
        Options.Applicative.switch
        (   Options.Applicative.long "cache"
        <>  Options.Applicative.help "Add fallback unprotected imports when using integrity checks purely for caching purposes"
        )

    parseCheck processed = fmap adapt switch
      where
        adapt True  = Check
        adapt False = Write

        switch =
            Options.Applicative.switch
            (   Options.Applicative.long "check"
            <>  Options.Applicative.help ("Only check if the input is " <> processed)
            )

    parseSchemasRecord =
        Options.Applicative.strOption
            (   Options.Applicative.long "schemas"
            <>  Options.Applicative.help "A record of schemas"
            <>  Options.Applicative.metavar "EXPR"
            )

    parseDirectoryTreeOutput =
        Options.Applicative.strOption
            (   Options.Applicative.long "output"
            <>  Options.Applicative.help "The destination path to create"
            <>  Options.Applicative.metavar "PATH"
            <>  Options.Applicative.action "directory"
            )

    parseNoted =
        Options.Applicative.switch
            (   Options.Applicative.long "noted"
            <>  Options.Applicative.help "Print `Note` constructors"
            )

    parseCache =
        Options.Applicative.switch
            (   Options.Applicative.long "cache"
            <>  Options.Applicative.help "Cache the hashed expression"
            )

-- | `ParserInfo` for the `Options` type
parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Interpreter for the Dhall language"
        <>  Options.Applicative.fullDesc
        )

noHeaders :: Import -> Import
noHeaders
    (Import { importHashed = ImportHashed { importType = Remote URL{ .. }, ..}, .. }) =
    Import { importHashed = ImportHashed { importType = Remote URL{ headers = Nothing, .. }, .. }, .. }
noHeaders i =
    i

-- | Run the command specified by the `Options` type
command :: Options -> IO ()
command (Options {..}) = do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    let rootDirectory = \case
            InputFile f   -> System.FilePath.takeDirectory f
            StandardInput -> "."

    let toStatus = Dhall.Import.emptyStatus . rootDirectory

    let getExpression = Dhall.Util.getExpression censor

    -- The characterSet detection used here only works on the source
    -- expression, before any transformation is applied. This helper is there
    -- make sure the detection is done on the correct expr.
    let getExpressionAndCharacterSet file = do
            expr <- getExpression file

            let characterSet = fromMaybe (detectCharacterSet expr) chosenCharacterSet

            return (expr, characterSet)

    let handle io =
            Control.Exception.catches io
                [ Handler handleTypeError
                , Handler handleImported
                , Handler handleExitCode
                ]
          where
            handleAll e = do
                let string = show (e :: SomeException)

                if not (null string)
                    then System.IO.hPutStrLn System.IO.stderr string
                    else return ()

                System.Exit.exitFailure

            handleTypeError e = Control.Exception.handle handleAll $ do
                let _ = e :: TypeError Src Void
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then
                        case censor of
                            Censor   -> Control.Exception.throwIO (CensoredDetailed (DetailedTypeError e))
                            NoCensor -> Control.Exception.throwIO (DetailedTypeError e)

                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        case censor of
                            Censor   -> Control.Exception.throwIO (Censored e)
                            NoCensor -> Control.Exception.throwIO e

            handleImported (Imported ps e) = Control.Exception.handle handleAll $ do
                let _ = e :: TypeError Src Void
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handleExitCode e =
                Control.Exception.throwIO (e :: ExitCode)

    let renderDoc :: Handle -> Doc Ann -> IO ()
        renderDoc h doc = do
            let stream = Dhall.Pretty.layout doc

            supportsANSI <- System.Console.ANSI.hSupportsANSI h
            let ansiStream =
                    if supportsANSI && not plain
                    then fmap annToAnsiStyle stream
                    else Pretty.unAnnotateS stream

            Pretty.renderIO h ansiStream
            Data.Text.IO.hPutStrLn h ""

    let render :: Pretty a => Handle -> CharacterSet -> Expr Src a -> IO ()
        render h characterSet expression = do
            let doc = Dhall.Pretty.prettyCharacterSet characterSet expression

            renderDoc h doc

    let writeDocToFile :: FilePath -> Doc ann -> IO ()
        writeDocToFile file doc = do
            let stream = Dhall.Pretty.layout (doc <> "\n")

            AtomicWrite.LazyText.atomicWriteFile file (Pretty.Text.renderLazy stream)

    handle $ case mode of
        Version ->
            putStrLn dhallVersionString

        Default {..} -> do
            if version
                then do
                    putStrLn dhallVersionString
                    Exit.exitSuccess
                else return ()

            (expression, characterSet) <- getExpressionAndCharacterSet file

            resolvedExpression <-
                Dhall.Import.loadRelativeTo (rootDirectory file) semanticCacheMode expression

            inferredType <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

            let normalizedExpression = Dhall.Core.normalize resolvedExpression

            let alphaNormalizedExpression =
                    if alpha
                    then Dhall.Core.alphaNormalize normalizedExpression
                    else normalizedExpression

            let annotatedExpression =
                    if annotate
                        then Annot alphaNormalizedExpression inferredType
                        else alphaNormalizedExpression

            case output of
                StandardOutput -> render System.IO.stdout characterSet annotatedExpression

                OutputFile file_ ->
                    writeDocToFile
                        file_
                        (Dhall.Pretty.prettyCharacterSet characterSet annotatedExpression)

        Resolve { resolveMode = Just Dot, ..} -> do
            expression <- getExpression file

            (Dhall.Import.Types.Status { _graph, _stack }) <-
                State.execStateT (Dhall.Import.loadWith expression) (toStatus file) { _semanticCacheMode = semanticCacheMode }

            let (rootImport :| _) = _stack
                imports = rootImport : map parent _graph ++ map child _graph
                importIds = Data.Map.fromList (zip imports [Text.Dot.userNodeId i | i <- [0..]])

            let dotNode (i, nodeId) =
                    Text.Dot.userNode
                        nodeId
                        [ ("label", Data.Text.unpack $ pretty (convert i))
                        , ("shape", "box")
                        , ("style", "rounded")
                        ]
                  where
                    convert = noHeaders . Dhall.Import.chainedImport

            let dotEdge (Depends parent child) =
                    case (Data.Map.lookup parent importIds, Data.Map.lookup child importIds) of
                        (Just from, Just to) -> from .->. to
                        _                    -> pure ()

            let dot = do Text.Dot.attribute ("rankdir", "LR")
                         mapM_ dotNode (Data.Map.assocs importIds)
                         mapM_ dotEdge _graph

            putStr . ("strict " <>) . Text.Dot.showDot $ dot

        Resolve { resolveMode = Just ListImmediateDependencies, ..} -> do
            expression <- getExpression file

            mapM_ (print . Pretty.pretty . noHeaders) expression

        Resolve { resolveMode = Just ListTransitiveDependencies, ..} -> do
            expression <- getExpression file

            (Dhall.Import.Types.Status { _cache }) <-
                State.execStateT (Dhall.Import.loadWith expression) (toStatus file) { _semanticCacheMode = semanticCacheMode }

            mapM_ print
                 .   fmap ( Pretty.pretty
                          . noHeaders
                          . Dhall.Import.chainedImport
                          )
                 .   reverse
                 .   Dhall.Map.keys
                 $   _cache

        Resolve { resolveMode = Nothing, ..} -> do
            (expression, characterSet) <- getExpressionAndCharacterSet file

            resolvedExpression <-
                Dhall.Import.loadRelativeTo (rootDirectory file) semanticCacheMode expression

            render System.IO.stdout characterSet resolvedExpression

        Normalize {..} -> do
            (expression, characterSet) <- getExpressionAndCharacterSet file

            resolvedExpression <- Dhall.Import.assertNoImports expression

            _ <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

            let normalizedExpression = Dhall.Core.normalize resolvedExpression

            let alphaNormalizedExpression =
                    if alpha
                    then Dhall.Core.alphaNormalize normalizedExpression
                    else normalizedExpression

            render System.IO.stdout characterSet alphaNormalizedExpression

        Type {..} -> do
            (expression, characterSet) <- getExpressionAndCharacterSet file

            resolvedExpression <-
                Dhall.Import.loadRelativeTo (rootDirectory file) semanticCacheMode expression

            inferredType <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

            if quiet
                then return ()
                else render System.IO.stdout characterSet inferredType

        Repl ->
            Dhall.Repl.repl
                (fromMaybe Unicode chosenCharacterSet) -- Default to Unicode if no characterSet specified
                explain

        Diff {..} -> do
            expression1 <- Dhall.inputExpr expr1

            expression2 <- Dhall.inputExpr expr2

            let diff = Dhall.Diff.diffNormalized expression1 expression2

            renderDoc System.IO.stdout (Dhall.Diff.doc diff)

            if Dhall.Diff.same diff
                then return ()
                else Exit.exitFailure

        Format {..} -> do
            when deprecatedInPlace $
                System.IO.hPutStrLn System.IO.stderr "Warning: the flag \"--inplace\" is deprecated"

            Dhall.Format.format Dhall.Format.Format{..}

        Freeze {..} -> do
            when deprecatedInPlace $
                System.IO.hPutStrLn System.IO.stderr "Warning: the flag \"--inplace\" is deprecated"

            let scope = if all_ then AllImports else OnlyRemoteImports

            let intent = if cache then Cache else Secure

            Dhall.Freeze.freeze outputMode transitivity inputs scope intent chosenCharacterSet commentControl censor

        Hash {..} -> do
            expression <- getExpression file

            resolvedExpression <-
                Dhall.Import.loadRelativeTo (rootDirectory file) UseSemanticCache expression

            _ <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

            let normalizedExpression =
                    Dhall.Core.alphaNormalize (Dhall.Core.normalize resolvedExpression)

            if cache
                then Dhall.Import.writeExpressionToSemanticCache normalizedExpression
                else return ()

            Data.Text.IO.putStrLn (Dhall.Import.hashExpressionToCode normalizedExpression)

        Lint { transitivity = transitivity0, ..} -> do
            when deprecatedInPlace $
                System.IO.hPutStrLn System.IO.stderr "Warning: the flag \"--inplace\" is deprecated"

            handleMultipleChecksFailed "lint" "linted" go inputs
          where
            go input = do
                let directory = case input of
                        StandardInput  -> "."
                        InputFile file -> System.FilePath.takeDirectory file

                let status = (Dhall.Import.emptyStatus directory)
                        { Dhall.Import._commentControl = commentControl }

                (inputName, originalText, transitivity) <- case input of
                    InputFile file -> do
                        text <- Data.Text.IO.readFile file

                        return (file, text, transitivity0)
                    StandardInput -> do
                        text <- Data.Text.IO.getContents

                        return ("(input)", text, NonTransitive)

                (Header header, parsedExpression) <-
                    Dhall.Util.getExpressionAndHeaderFromStdinText commentControl censor inputName originalText

                let characterSet = fromMaybe (detectCharacterSet parsedExpression) chosenCharacterSet

                case transitivity of
                    Transitive ->
                        for_ parsedExpression $ \import_ -> do
                            maybeFilepath <- Dhall.Import.dependencyToFile status import_

                            for_ maybeFilepath $ \filepath ->
                                go (InputFile filepath)

                    NonTransitive ->
                        return ()

                let lintedExpression = Dhall.Lint.lint parsedExpression

                let doc =   Pretty.pretty header
                        <>  Dhall.Pretty.prettyCharacterSet characterSet lintedExpression

                let stream = Dhall.Pretty.layout doc

                let modifiedText = Pretty.Text.renderStrict stream <> "\n"

                case outputMode of
                    Write -> do
                        case input of
                            InputFile file ->
                                if originalText == modifiedText
                                    then return ()
                                    else writeDocToFile file doc

                            StandardInput ->
                                renderDoc System.IO.stdout doc

                        return (Right ())

                    Check ->
                        return $
                            if originalText == modifiedText
                                then Right ()
                                else Left CheckFailed{..}

        Encode {..} -> do
            expression <- getExpression file

            let bytes = Dhall.Binary.encodeExpression (Dhall.Core.denote expression)

            if json
                then do
                    let decoder = Codec.CBOR.JSON.decodeValue False

                    (_, value) <- Dhall.Core.throws (Codec.CBOR.Read.deserialiseFromBytes decoder bytes)

                    let jsonBytes = Data.Aeson.Encode.Pretty.encodePretty value

                    Data.ByteString.Lazy.Char8.putStrLn jsonBytes

                else
                    Data.ByteString.Lazy.putStr bytes

        Decode {..} -> do
            bytes <-
                case file of
                    InputFile f   -> Data.ByteString.Lazy.readFile f
                    StandardInput -> Data.ByteString.Lazy.getContents

            expression <-
                if json
                    then do
                        value <- case Data.Aeson.eitherDecode' bytes of
                            Left  string -> fail string
                            Right value  -> return value

                        let encoding = Codec.CBOR.JSON.encodeValue value

                        let cborgBytes = Codec.CBOR.Write.toLazyByteString encoding

                        Dhall.Core.throws (Dhall.Binary.decodeExpression cborgBytes)
                    else
                        Dhall.Core.throws (Dhall.Binary.decodeExpression bytes)


            if quiet
                then return ()
                else do
                    let doc =
                            Dhall.Pretty.prettyCharacterSet
                                (fromMaybe Unicode chosenCharacterSet) -- default to Unicode
                                (Dhall.Core.renote expression :: Expr Src Import)

                    renderDoc System.IO.stdout doc

        Text {..} -> do
            expression <- getExpression file

            resolvedExpression <-
                Dhall.Import.loadRelativeTo (rootDirectory file) UseSemanticCache expression

            _ <- Dhall.Core.throws (Dhall.TypeCheck.typeOf (Annot resolvedExpression Dhall.Core.Text))

            let normalizedExpression = Dhall.Core.normalize resolvedExpression

            case normalizedExpression of
                Dhall.Core.TextLit (Dhall.Core.Chunks [] text) ->
                    let write = case output of
                          StandardOutput -> Data.Text.IO.putStr
                          OutputFile file_ -> Data.Text.IO.writeFile file_
                    in write text
                _ -> do
                    let invalidDecoderExpected :: Expr Void Void
                        invalidDecoderExpected = Dhall.Core.Text

                    let invalidDecoderExpression :: Expr Void Void
                        invalidDecoderExpression = normalizedExpression

                    Control.Exception.throwIO (Dhall.InvalidDecoder {..})

        Tags {..} -> do
            tags <- Dhall.Tags.generate input suffixes followSymlinks

            case output of
                OutputFile file ->
                    System.IO.withFile file System.IO.WriteMode (`Data.Text.IO.hPutStr` tags)

                StandardOutput -> Data.Text.IO.putStrLn tags

        DirectoryTree {..} -> do
            expression <- getExpression file

            resolvedExpression <-
                Dhall.Import.loadRelativeTo (rootDirectory file) UseSemanticCache expression

            _ <- Dhall.Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

            let normalizedExpression = Dhall.Core.normalize resolvedExpression

            DirectoryTree.toDirectoryTree path normalizedExpression

        Dhall.Main.Schemas{..} ->
            Dhall.Schemas.schemasCommand Dhall.Schemas.Schemas{ input = file, ..}

        SyntaxTree {..} -> do
            expression <- getExpression file

            if noted then
                Text.Pretty.Simple.pPrintNoColor expression
            else
                let denoted :: Expr Void Import
                    denoted = Dhall.Core.denote expression
                in Text.Pretty.Simple.pPrintNoColor denoted

-- | Entry point for the @dhall@ executable
main :: IO ()
main = do
    options <- Options.Applicative.execParser parserInfoOptions

    Dhall.Main.command options
