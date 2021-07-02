{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall tags@ command

module Dhall.Tags
    ( generate
    ) where

import Control.Exception  (SomeException (..), handle)
import Data.List          (foldl', isSuffixOf)
import Data.Maybe         (fromMaybe)
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
import Dhall.Map          (foldMapWithKey)
import Dhall.Parser       (exprFromText)
import Dhall.Src          (Src (srcStart))
import Dhall.Syntax       (Binding (..), Expr (..), RecordField (..))
import Dhall.Util         (Input (..))
import System.FilePath    (takeFileName, (</>))
import Text.Megaparsec    (sourceColumn, sourceLine, unPos)

import qualified Data.ByteString  as BS (length)
import qualified Data.Map         as M
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import qualified System.Directory as SD

{-
    Documentation for the etags format is not very informative and not very correct.
    You can find some documentation here:
    https://en.wikipedia.org/wiki/Ctags#Etags_2
    and you can also check the source code here:
    http://cvs.savannah.gnu.org/viewvc/vtags/vtags/vtags.el?view=markup
-}

data LineColumn = LC
    { _lcLine :: Int
      -- ^ line number, starting from 1, where to find the tag
    , _lcColumn :: Int
      -- ^ column of line where tag is
    } deriving (Eq, Ord, Show)

data LineOffset = LO
    { loLine :: Int
      -- ^ line number, starting from 1, where to find the tag
    , loOffset :: Int
      -- ^ byte offset from start of file. Not sure if any editor uses it
    } deriving (Eq, Ord, Show)

newtype Tags = Tags (M.Map FilePath [(LineOffset, Tag)])

instance Semigroup Tags where
    (Tags ts1) <> (Tags ts2) = Tags (M.unionWith (<>) ts1 ts2)

instance Monoid Tags where
    mempty = Tags M.empty

{-| For example, for the line: @let foo = \"foo\"@ the tag is:
    > Tag "let " "foo"
-}
data Tag = Tag
    { tagPattern :: Text
      -- ^ In vtags source code this field is named \"pattern\" and EMacs used it as
      --   a regex pattern to locate line with tag. It's looking for ^<tag pattern>.
      --   Looks like vi is not using it.
    , tagName :: Text
      -- ^ text, that editor compare with selected text. So it's really name of entity
    } deriving (Show)

type LineNumber = Int

type ByteOffset = Int

{-| Generate etags for Dhall expressions
-}
generate
    :: Input
    -- ^ Where to look for files. This can be a directory name (@.@ for example),
    --   a file name or `StandardInput`. If `StandardInput`, then this will wait for
    --   file names from @STDIN@.
    --   This way someone can combine tools in @bash@ to send, for example, output from
    --   @find@ to the input of @dhall tags@.
    -> Maybe [Text]
    -- ^ List of suffixes for dhall files or Nothing to check all files
    -> Bool
    -- ^ Flag if `generate` should follow symlinks
    -> IO Text
    -- ^ Content for tags file
generate inp sxs followSyms = do
    files <- inputToFiles followSyms (map T.unpack <$> sxs) inp
    tags <- traverse (\f -> handle (\(SomeException _) -> return mempty)
                                   (fileTags f <$> TIO.readFile f)) files
    return (showTags . mconcat $ tags)

{-| Find tags in Text (second argument) and generates a list of them
    To make tags for filenames that works in both emacs and vi, add two initial tags.
    First for @filename@ for vi and second with @/filename@ for emacs.
    Other tags are working for both.
-}
fileTags :: FilePath -> Text -> Tags
fileTags f t = Tags (M.singleton f
                    (initialMap <> getTagsFromText t))
    where initialViTag = (LO 1 1, Tag "" (T.pack . takeFileName $ f))
          initialEmacsTag = (LO 1 1, Tag "" ("/" <> (T.pack . takeFileName) f))
          initialMap = [initialViTag, initialEmacsTag]

getTagsFromText :: Text -> [(LineOffset, Tag)]
getTagsFromText t = case exprFromText "" t of
    Right expr -> fixPosAndDefinition t (getTagsFromExpr expr)
    _ -> mempty

{-| Used to update tag position and to build tag from term.
    After getTagsFromExpr line and column in line are in @LineColumn@ for each tag.
    And tagPattern is not added.
    Emacs use tag pattern to check if tag is on line. It compares line from start
    with tag pattern and in case they are the same, relocate user.
    fixPosAndDefinition change position to line and byte offset (@LineOffset@) and
    add tag pattern. For example, for Dhall string:

    >>> let dhallSource = "let foo = \"bar\"\nlet baz = \"qux\""

    Input for this function is:

    >>> foundTerms = [(LC 1 4, "foo"), (LC 2 4, "baz")]

    And:

    >>> fixPosAndDefinition dhallSource foundTerms
    [(LO {loLine = 1, loOffset = 5},Tag {tagPattern = "let foo ", tagName = "foo"}),(LO {loLine = 2, loOffset = 21},Tag {tagPattern = "let baz ", tagName = "baz"})]

    where 21 is byte offset from file start.
-}
fixPosAndDefinition :: Text -> [(LineColumn, Text)] -> [(LineOffset, Tag)]
fixPosAndDefinition t = foldMap (\(LC ln c, term) ->
             let (ln', offset, tPattern) = fromMaybe (fallbackInfoForText ln c)
                                                     (infoForText term ln)
             in [(LO ln' offset, Tag tPattern term)])
    where mls :: M.Map Int (Text, Int)
          -- ^ mls is map that for each line has length of file before this map and line content.
          --   In example above, first line is 15 bytes long and '\n', mls contain:
          --   (1, (16, "let foo = "bar"")
          --   That allow us to get byte offset easier.
          mls = M.fromList . fst . foldl' processLine ([], 0) . zip [1..] $ T.lines t

          {-| processLine is a worker for `foldl` that generates the list of lines with
              byte offsets from the start of the first line from a list of lines
          -}
          processLine
              :: ([(LineNumber, (Text, ByteOffset))], ByteOffset)
              -- ^ previous result and byte offset for the start of current line
              -> (LineNumber, Text)
              -> ([(LineNumber, (Text, ByteOffset))], ByteOffset)
              -- ^ next result, where new line was added and byte offset for next line
          processLine (numberedLinesWithSizes, bytesBeforeLine) (n, line) =
              ((n, (line, bytesBeforeLine)): numberedLinesWithSizes, bytesBeforeNextLine)
              where bytesBeforeNextLine = bytesBeforeLine + lengthInBytes line + 1

          lineFromMap ln = fromMaybe ("", 0) (ln `M.lookup` mls)

          lengthInBytes = BS.length . encodeUtf8

          {-| get information about term from map of lines
              In most cases, @LineColumn@ after `getTagsFromExpr` points to byte before term.
              It's better to have term in term pattern, so this function finds and updates
              line number and byte offset and generate pattern.
          -}
          infoForText
              :: Text
              -- ^ term to find
              -> Int
              -- ^ line where to start
              -> Maybe (Int, Int, Text)
              -- ^ (Line number, byte offset, pattern to find term in file)
          infoForText term ln
              | ln <= 0 = Nothing
              | T.null part2 = infoForText term (ln - 1)
              | otherwise = Just (ln, lsl + 1 + lengthInBytes part1, part1 <> termAndNext)
              where (l, lsl) = lineFromMap ln
                    (part1, part2) = T.breakOn term l
                    termAndNext = T.take (T.length term + 1) part2

          fallbackInfoForText ln c = (ln, lsl + 1 + lengthInBytes pat, pat)
              where (l, lsl) = lineFromMap ln
                    pat = T.take c l

getTagsFromExpr :: Expr Src a -> [(LineColumn, Text)]
getTagsFromExpr = go (LC 0 0) []
    where go lpos mts = \case
              (Let b e) -> go lpos (mts <> parseBinding lpos b) e
              (Annot e1 e2) -> go lpos (go lpos mts e1) e2
              (Record mr) -> mts <> tagsFromDhallMap lpos (recordFieldValue <$> mr)
              (RecordLit mr) -> mts <> tagsFromDhallMap lpos (recordFieldValue <$> mr)
              (Union mmr) -> mts <> tagsFromDhallMapMaybe lpos mmr
              (Note s e) -> go (srcToLineColumn s) mts e
              _ -> mts

          tagsFromDhallMap lpos = foldMapWithKey (tagsFromDhallMapElement lpos)

          tagsFromDhallMapMaybe lpos = foldMapWithKey (\k -> \case
              Just e -> tagsFromDhallMapElement lpos k e
              _ -> [(lpos, k)])

          tagsFromDhallMapElement lpos k e = go pos [(pos, k)] e
              where pos = firstPosFromExpr lpos e

          parseBinding :: LineColumn -> Binding Src a -> [(LineColumn, Text)]
          parseBinding lpos b = go pos [(pos, variable b)] (value b)
            where
              pos = maybe lpos srcToLineColumn (variableSrc b)

srcToLineColumn :: Src -> LineColumn
srcToLineColumn s = LC line column
    where ssp = srcStart s
          line = unPos . sourceLine $ ssp
          column = unPos . sourceColumn $ ssp

firstPosFromExpr :: LineColumn -> Expr Src a -> LineColumn
firstPosFromExpr lpos = \case
    (Note s _) -> srcToLineColumn s
    _ -> lpos

showTags :: Tags -> Text
showTags (Tags ts) = T.concat . map (uncurry showFileTags) . M.toList $ ts

showFileTags :: FilePath -> [(LineOffset, Tag)] -> T.Text
showFileTags f ts = "\x0c\n" <> T.pack f <> "," <> (showInt . T.length) cs <> "\n" <> cs
    where cs = T.concat . map (uncurry showPosTag) $ ts

showPosTag :: LineOffset -> Tag -> Text
showPosTag lo tag = def <>"\x7f" <> name <> "\x01" <> showInt line <>
                    "," <> showInt offset <> "\n"
    where line = loLine lo
          offset = loOffset lo
          def = tagPattern tag
          name = tagName tag

showInt :: Int -> Text
showInt = T.pack . show

{-| Generate list of files for a given `Input`
-}
inputToFiles
    :: Bool
    -- ^ If `True`, this function will follow  symbolic links
    -> Maybe [String]
    -- ^ List of suffixes. If `Nothing`, all files will be returned.
    --   This parameter only works when the `Input` is an `InputFile` and point to a directory.
    -> Input
    -> IO [ FilePath ]
    --   List of files.
inputToFiles _ _ StandardInput = lines <$> getContents
inputToFiles followSyms suffixes (InputFile path) = go path
    where go p = do
                   isD <- SD.doesDirectoryExist p
                   isSL <- isSymLink
                   if isD
                     then if isSL && not followSyms
                            then return []
                            else do
                                   -- filter . .. and hidden files .*
                                   contents <- fmap (filter ((/=) '.' . head))
                                                    (SD.getDirectoryContents p)
                                   concat <$> mapM (go . (</>) p) contents
                     else return [p | matchingSuffix || p == path]
               where matchingSuffix = maybe True (any (`isSuffixOf` p)) suffixes
                     isSymLink = SD.pathIsSymbolicLink p
