{-| Contains all the functions that generate documentation

    We should always try to do as little work as possible in an `IO` context.
    To do so, just wrap your function in `IO` if you need to do I/O operations,
    and make pure functions receive that IO result as an input
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Dhall.Docs.Core
    ( -- * Core functionality
      generateDocs
    , generateDocsPure
    , GeneratedDocs(..)

      -- * Comment parsing
    , module Dhall.Docs.Comment
    ) where

import Control.Applicative        (Alternative (..))
import Control.Monad.Writer.Class (MonadWriter)
import Data.ByteString            (ByteString)
import Data.Function              (on)
import Data.Map.Strict            (Map)
import Data.Text                  (Text)
import Data.Void                  (Void)
import Dhall.Core
    ( Binding (..)
    , Expr (..)
    , Import
    , MultiLet (..)
    , Var (..)
    , denote
    )
import Dhall.Docs.Comment
import Dhall.Docs.Embedded
import Dhall.Docs.Html
import Dhall.Docs.Markdown
import Dhall.Docs.Store
import Dhall.Parser
    ( Header (..)
    , ParseError (..)
    , WhitespaceControl (UnsupportedCommentsPermitted)
    , exprAndHeaderFromText
    )
import Dhall.Pretty               (CharacterSet)
import Dhall.Src                  (Src)
import Path                       (Abs, Dir, File, Path, Rel, (</>))
import Text.Megaparsec            (ParseErrorBundle (..))

import qualified Control.Applicative        as Applicative
import qualified Control.Monad
import qualified Control.Monad.Writer.Class as Writer
import qualified Data.ByteString
import qualified Data.List
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Map.Strict            as Map
import qualified Data.Map.Merge.Strict      as Map.Merge
import qualified Data.Maybe
import qualified Data.Maybe                 as Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO               as Text.IO
import qualified Data.Text.Lazy             as Text.Lazy
import qualified Dhall.Core
import qualified Lucid
import qualified Path
import qualified Path.IO
import qualified System.FilePath            as FilePath
import qualified Text.Megaparsec

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Path (reldir)

-- | The result of the doc-generator pure component
data GeneratedDocs a = GeneratedDocs [DocsGenWarning] a
    deriving (Show)

instance Functor GeneratedDocs where
    fmap f (GeneratedDocs w a) = GeneratedDocs w (f a)

instance Applicative GeneratedDocs where
    pure = GeneratedDocs []

    GeneratedDocs w f <*> GeneratedDocs w' a = GeneratedDocs (w <> w') (f a)

instance Monad GeneratedDocs where
    GeneratedDocs w a >>= f =
        let GeneratedDocs w' b = f a
            in GeneratedDocs (w <> w') b

instance MonadWriter [DocsGenWarning] GeneratedDocs where
    tell w = GeneratedDocs w ()

    listen (GeneratedDocs w a) = GeneratedDocs w (a, w)
    pass (GeneratedDocs w (a, f)) = GeneratedDocs (f w) a

data DocsGenWarning
    = InvalidDhall (Text.Megaparsec.ParseErrorBundle Text Void)
    | InvalidMarkdown MarkdownParseError
    | DhallDocsCommentError (Path Rel File) CommentParseError

warn :: String
warn = "\n\ESC[1;33mWarning\ESC[0m: "

instance Show DocsGenWarning where
    show (InvalidDhall err) =
        warn <> "Invalid Input\n\n" <>
        Text.Megaparsec.errorBundlePretty err <>
        "... documentation won't be generated for this file"

    show (InvalidMarkdown MarkdownParseError{..}) =
        warn <>"Header comment is not markdown\n\n" <>
        Text.Megaparsec.errorBundlePretty unwrap <>
        "The original non-markdown text will be pasted in the documentation"

    show (DhallDocsCommentError path err) =
        warn <> Path.fromRelFile path <> specificError
      where
        specificError = case err of
            MissingNewlineOnBlockComment -> ": After the `|` marker of a block comment " <>
                "there must be a newline (either \\n or \\r\\n)"

            SeveralSubseqDhallDocsComments -> ": Two dhall-docs comments in the same " <>
                "comment section are forbidden"

            BadSingleLineCommentsAlignment -> ": dhall-docs's single line comments " <>
                "must be aligned"

            BadPrefixesOnSingleLineComments -> ": dhall-docs's single line comments " <>
                "must have specific prefixes:" <>
                "* For the first line: \"--| \"\n" <>
                "* For the rest of the linse: \"--  \""

-- | Extracted text from from Dhall file's comments
newtype FileComments = FileComments
    { headerComment :: Maybe DhallDocsText -- ^ 'Nothing' if no comment or if invalid
    } deriving (Show)

-- | Represents a Dhall file that can be rendered as documentation.
--   If you'd like to improve or add features to a .dhall documentation page,
--   add that extra information here.
data DhallFile = DhallFile
    { path :: Path Rel File             -- ^ Path of the file
    , contents :: Text                  -- ^ File contents
    , expr :: Expr Src Import           -- ^ Parsed AST from 'contents'
    , mType :: Maybe (Expr Void Import) -- ^ Type of the parsed expression,
                                        --   extracted from the source code
    , examples :: [Expr Void Import]    -- ^ Examples extracted from assertions
                                        --   in the file
    , fileComments :: FileComments
    } deriving (Show)

{-| Takes a list of files paths with their contents and returns the list of
    valid `DhallFile`s.

    Returned files contains all the information to be used on `Html ()`
    generation.

    The result is sorted by `path`
-}
getAllDhallFiles :: [(Path Rel File, ByteString)] -> GeneratedDocs [DhallFile]
getAllDhallFiles = fmap Maybe.catMaybes . mapM toDhallFile . foldr validFiles [] . filter hasDhallExtension
  where
    hasDhallExtension :: (Path Rel File, a) -> Bool
    hasDhallExtension (absFile, _) = case Path.splitExtension absFile of
        Nothing -> False
        Just (_, ext) -> ext == ".dhall"

    validFiles :: (Path Rel File, ByteString) -> [(Path Rel File, Text)] -> [(Path Rel File, Text)]
    validFiles (relFile, content) xs = case Data.Text.Encoding.decodeUtf8' content of
        Left _ -> xs
        Right textContent -> (relFile, textContent) : xs

    toDhallFile :: (Path Rel File, Text) -> GeneratedDocs (Maybe DhallFile)
    toDhallFile (relFile, contents) =
        case exprAndHeaderFromText UnsupportedCommentsPermitted (Path.fromRelFile relFile) contents of
            Right (Header header, expr) -> do
                let denoted = denote expr :: Expr Void Import

                headerContents <-
                    case parseSingleDhallDocsComment (Path.fromRelFile relFile) header of
                        Nothing -> return Nothing
                        Just (Left errs) -> do
                            Writer.tell $ map (DhallDocsCommentError relFile) errs
                            return Nothing
                        Just (Right c) -> return $ Just c

                return $ Just $ DhallFile
                    { expr, contents
                    , path = relFile
                    , mType = extractTypeIfInSource denoted
                    , examples = examplesFromAssertions denoted
                    , fileComments = FileComments headerContents
                    }
            Left ParseError{..} -> do
                Writer.tell [InvalidDhall unwrap]
                return Nothing

    bindings :: Expr Void Import -> [Binding Void Import]
    bindings expr = case expr of
        Let b@Binding{} e ->
            let MultiLet bs _ = Dhall.Core.multiLet b e
            in NonEmpty.toList bs
        _ -> []

    extractTypeIfInSource :: Expr Void Import -> Maybe (Expr Void Import)
    extractTypeIfInSource expr =
            fromOrdinaryAnnotation expr
        <|> fromLetBindingAnnotation
      where
        fromOrdinaryAnnotation (Let _ e)    = fromOrdinaryAnnotation e
        fromOrdinaryAnnotation (Annot _ _T) = pure _T
        fromOrdinaryAnnotation  _           = empty

        fromLetBindingAnnotation = do
            V name index <- maybeNameInLet expr
            (Binding _ _ _ _ (Just (_, exprType)) _ _) <-
                getLetBindingWithIndex index $ getLetBindingsWithName name
            return exprType

        -- | For an expression of the form @let x0 = y0 let x1 = y1 ... in e@
        --   where @e@ is a variable, maybeNameInLet returns the variable name.
        maybeNameInLet :: Expr Void Import -> Maybe Var
        maybeNameInLet (Var v@(V _ _)) = Just v
        maybeNameInLet (Let _ e) = maybeNameInLet e
        maybeNameInLet _ = Nothing


        {-| For an expression of the form @let x0 = y0 let x1 = y1 ... in e@
            and a variable name @v@, this returns every @xi@ that is equal to
            v in the reverse order of the source code.

            For example, take a file like this:

        >   let x = 1
        >   let y = 2
        >   let z = 3
        >   in x + y + z

            ... this will return the bindings in this order: [z, y, x]

            Only the "global" level of the file is analyzed
        -}
        getLetBindingsWithName :: Text -> [Binding Void Import]
        getLetBindingsWithName name = filter bindName $ reverse $ bindings expr
          where
            bindName (Binding _ x _ _ _ _ _) = x == name


        getLetBindingWithIndex :: Int -> [Binding Void Import] -> Maybe (Binding Void Import)
        getLetBindingWithIndex i bs =
            case drop i bs of
                [] -> Nothing
                binding : _ -> Just binding

    examplesFromAssertions :: Expr Void Import -> [Expr Void Import]
    examplesFromAssertions expr = Maybe.mapMaybe fromAssertion values
      where
        values :: [Expr Void Import]
        values = map value $ bindings expr

        fromAssertion :: Expr Void Import -> Maybe (Expr Void Import)
        fromAssertion (Assert e) =  Just e
        fromAssertion _ = Nothing


{-| Given a relative path, returns as much @..\/@ misdirections as needed
    to go to @.@

>>> resolveRelativePath [reldir|.|]
""
>>> resolveRelativePath [reldir|a|]
"../"
>>> resolveRelativePath [reldir|a/b/c|]
"../../../"
-}
resolveRelativePath :: Path Rel Dir -> FilePath
resolveRelativePath currentDir =
    case FilePath.dropTrailingPathSeparator $ Path.fromRelDir currentDir of
        "." -> ""
        _ -> "../" <> resolveRelativePath (Path.parent currentDir)

-- | Generates `Text` from the HTML representation of a `DhallFile`
makeHtml
    :: Maybe Text           -- ^ Base import URL
    -> Text                 -- ^ Package name
    -> CharacterSet         -- ^ Output encoding
    -> DhallFile            -- ^ Parsed header
    -> GeneratedDocs Text
makeHtml baseImportUrl packageName characterSet DhallFile {..} = do
    let relativeResourcesPath = resolveRelativePath $ Path.parent path
    let strippedHeader = Maybe.maybe "" unDhallDocsText (headerComment fileComments)
    headerAsHtml <-
        case markdownToHtml path strippedHeader of
            Left err -> do
                Writer.tell [InvalidMarkdown err]
                return $ Lucid.toHtml strippedHeader
            Right html -> return html

    let htmlAsText = Text.Lazy.toStrict $ Lucid.renderText $ dhallFileToHtml
            path
            contents
            expr
            examples
            headerAsHtml
            DocParams { relativeResourcesPath, packageName, characterSet, baseImportUrl }

    return htmlAsText

{-| Create an @index.html@ file on each available folder in the input.

    Each @index.html@ lists the files and directories of its directory. Listed
    directories will be compacted as much as it cans to improve readability.

    For example, take the following directory-tree structure

    > .
    > ├── a
    > │   └── b
    > │       └── c
    > │           └── b.dhall
    > └── a.dhall

    To improve navigation, the index at @./index.html@ should list
    @a/b/c@ and no @index.html@ should be generated inside of `a/` or
    `a/b/`, but yes on `a/b/c/` in the last one there is the @b.dhall@ file
-}
createIndexes
    :: Maybe Text
    -> Text
    -> CharacterSet
    -> [DhallFile]
    -> [(Path Rel File, Text)]
createIndexes baseImportUrl packageName characterSet dhallFiles = map toIndex dirToDirsAndFilesMapAssocs
  where
    -- Files grouped by their directory
    dirToFilesMap :: Map (Path Rel Dir) [DhallFile]
    dirToFilesMap = Map.unionsWith (<>) $ map toMap $ Data.List.sortBy (compare `on` path) dhallFiles
      where
        toMap :: DhallFile -> Map (Path Rel Dir) [DhallFile]
        toMap dhallFile = Map.singleton (Path.parent $ path dhallFile) [dhallFile]

    {-  This is used to compute the list of exported packages on each folder.
        We try to compress the folders as much as we can. See `createIndexes`
        documentation to get more information.
    -}
    dirToDirsMap :: Map (Path Rel Dir) [Path Rel Dir]
    dirToDirsMap = foldr cons Map.empty dirs
      where
        dirs = filter keep (Map.keys dirToFilesMap)
          where
            keep reldir = Path.parent reldir /= reldir

        cons d = Map.insertWith (<>) (Path.parent d) [d]

    dirToDirsAndFilesMapAssocs :: [(Path Rel Dir, ([DhallFile], [Path Rel Dir]))]
    dirToDirsAndFilesMapAssocs = Map.assocs $
        Map.Merge.merge
            (Map.Merge.mapMissing onlyFiles)
            (Map.Merge.mapMissing onlyDirectories)
            (Map.Merge.zipWithMatched both)
            dirToFilesMap
            dirToDirsMap
      where
        onlyFiles       _ files             = (files, []         )
        onlyDirectories _       directories = ([]   , directories)
        both            _ files directories = (files, directories)

    toIndex :: (Path Rel Dir, ([DhallFile], [Path Rel Dir])) -> (Path Rel File, Text)
    toIndex (indexDir, (files, dirs)) =
        (indexDir </> $(Path.mkRelFile "index.html"), Text.Lazy.toStrict $ Lucid.renderText html)
      where
        html = indexToHtml
            indexDir
            (map (\DhallFile{..} -> (stripPrefix $ addHtmlExt path, mType)) files)
            (map stripPrefix dirs)
            DocParams { relativeResourcesPath = resolveRelativePath indexDir, packageName, characterSet, baseImportUrl }

        stripPrefix :: Path Rel a -> Path Rel a
        stripPrefix relpath =
            if Path.toFilePath relpath == Path.toFilePath indexDir then relpath
            else Data.Maybe.fromMaybe (fileAnIssue "Bug+with+stripPrefix")
                $ Path.stripProperPrefix indexDir relpath

-- | Takes a file and adds an @.html@ file extension to it
addHtmlExt :: Path Rel File -> Path Rel File
addHtmlExt relFile =
    Data.Maybe.fromMaybe (fileAnIssue "addHtmlExt") $ Path.addExtension ".html" relFile

-- | If you're wondering the GitHub query params for issue creation:
-- https://docs.github.com/en/github/managing-your-work-on-github/about-automation-for-issues-and-pull-requests-with-query-parameters
fileAnIssue :: Text -> a
fileAnIssue titleName =
    error $ "\ESC[1;31mError\ESC[0m Documentation generator bug\n\n" <>

            "Explanation: This error message means that there is a bug in the " <>
            "Dhall Documentation generator. You didn't did anything wrong, but " <>
            "if you would like to see this problem fixed then you should report " <>
            "the bug at:\n\n" <>

            "https://github.com/dhall-lang/dhall-haskell/issues/new?labels=dhall-docs,bug\n\n" <>

            "explaining your issue and add \"" <> Data.Text.unpack titleName <> "\" as error code " <>
            "so we can find the proper location in the source code where the error happened\n\n" <>

            "Please, also include your package in the issue. It can be in:\n\n" <>
            "* A compressed archive (zip, tar, etc)\n" <>
            "* A git repository, preferably with a commit reference"

{-| Generate all of the docs for a package. This function does all the `IO ()`
    related tasks to call `generateDocsPure`
-}
generateDocs
    :: Path Abs Dir -- ^ Input directory
    -> Path Abs Dir -- ^ Link to be created to the generated documentation
    -> Maybe Text   -- ^ Base import URL
    -> Text         -- ^ Package name, used in some HTML titles
    -> CharacterSet -- ^ Output encoding
    -> IO ()
generateDocs inputDir outLink baseImportUrl packageName characterSet = do
    (_, absFiles) <- Path.IO.listDirRecur inputDir
    contents <- mapM (Data.ByteString.readFile . Path.fromAbsFile) absFiles
    strippedFiles <- mapM (Path.stripProperPrefix inputDir) absFiles
    let GeneratedDocs warnings docs = generateDocsPure baseImportUrl packageName characterSet $ zip strippedFiles contents
    mapM_ print warnings
    if null docs then
        putStrLn $
            "No documentation was generated because no file with .dhall " <>
            "extension was found"
    else Path.IO.withSystemTempDir "dhall-docs" $ \tempDir -> do
        copyDataDir tempDir
        mapM_ (writeGenFile tempDir) docs

        outputHash <- makeHashForDirectory tempDir
        outDir <- Applicative.liftA2 (</>)
                    getDocsHomeDirectory
                    (Path.parseRelDir
                        $ show outputHash <> "-" <> Data.Text.unpack packageName)

        Path.IO.copyDirRecur tempDir outDir
        Path.IO.createDirLink outDir outLink
  where
    writeGenFile :: Path Abs Dir -> (Path Rel File, Text) -> IO ()
    writeGenFile outDir (relFile, contents) = do
        Path.IO.ensureDir (outDir </> Path.parent relFile)
        Text.IO.writeFile (Path.fromAbsFile $ outDir </> relFile) contents

    copyDataDir :: Path Abs Dir -> IO ()
    copyDataDir outDir = do
        dataDir <- getDataDir
        Control.Monad.forM_ dataDir $ \(filename, contents) -> do
            let finalPath = Path.fromAbsFile $ outDir </> filename
            Data.ByteString.writeFile finalPath contents

{-| Generates all the documentation of dhall package in a pure way i.e.
    without an `IO` context. This let you generate documentation from a list of
    dhall-files without saving them to the filesystem.

    If you want the `IO` version of this function, check `generateDocs`
-}
generateDocsPure
    :: Maybe Text              -- ^ Base import URL
    -> Text                    -- ^ Package name
    -> CharacterSet            -- ^ Output encoding
    -> [(Path Rel File, ByteString)] -- ^ (Input file, contents)
    -> GeneratedDocs [(Path Rel File, Text)]
generateDocsPure baseImportUrl packageName characterSet inputFiles = go
  where
    go :: GeneratedDocs [(Path Rel File, Text)]
    go = do
        dhallFiles <- getAllDhallFiles inputFiles
        htmls <- mapM (makeHtml baseImportUrl packageName characterSet) dhallFiles
        let indexes = createIndexes baseImportUrl packageName characterSet dhallFiles
        return (zip (map (addHtmlExt . path) dhallFiles) htmls <> indexes)
