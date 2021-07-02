{-| Provides utilities to parse Dhall comments
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Dhall.Docs.Comment
    ( parseComments
    , CommentParseError(..)
    , DhallDocsText
    , parseSingleDhallDocsComment
    , unDhallDocsText
    ) where

import Control.Applicative (many, some, (<|>))
import Data.Functor        (void)
import Data.List.NonEmpty  (NonEmpty (..), (<|))
import Data.Text           (Text)
import Dhall.Docs.Util
import Dhall.Parser        (Parser (..), runParser, WhitespaceControl (UnsupportedCommentsPermitted))
import Text.Megaparsec     (SourcePos, (<?>))

import qualified Data.Either
import qualified Data.Foldable
import qualified Data.List.NonEmpty      as NonEmpty
import qualified Data.Maybe              as Maybe
import qualified Data.Text
import qualified Dhall.Parser.Token      as Token
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Pos     as Megaparsec.Pos

-- | For explanation of this data-type see 'DhallComment'
data CommentType = DhallDocsComment | MarkedComment | RawComment

type ListOfSingleLineComments = NonEmpty (SourcePos, Text)

{-| Internal representation of Dhall comments. Text is always stripped from
    whitespace from both the start and begin of the string

    * If @a = 'DhallDocsComment'@ then comment is valid to extract its contents
    and be rendered on documentation
    * If @a = 'MarkedComment'@ then comment has the @|@ marker that
    @dhall-docs@ will be aware of, but this comment may not be a @dhall-docs@
    comment
    * If @a = `RawComment`@ then the comment is a raw comment
-}
data DhallComment (a :: CommentType)
    -- | A single block comment: starting from @{-@ and ending in @-}@
    = BlockComment Text
    {-| A group of subsequent single line comment, each one starting from @--@
        and ending in the last character to the end of the line. Each one keeps
        its 'SourcePos' to validate indentation.

        A property of 'SingleLineComments' is that the 'sourceLine's in the
        'NonEmpty (SourcePos, Text)' are in strictly-increasing order /and/
        the difference between the 'sourceLine' of any adyacent pair is @1@.

        Note that several @dhall-docs@ comments maybe inside a single 'SingleLineComments'
    -}
    | SingleLineComments ListOfSingleLineComments
    deriving Show

-- | Extracted text from a valid @dhall-docs@ comment
newtype DhallDocsText = DhallDocsText Text
    deriving Show

unDhallDocsText :: DhallDocsText -> Text
unDhallDocsText (DhallDocsText t) = t

-- | A mirror of "Dhall.Parser.Token".'Dhall.Parser.Token.lineComment' but
--   returning a 'DhallComment'
lineCommentParser :: Parser (NonEmpty (DhallComment 'RawComment))
lineCommentParser = do
    (l : ls) <- some singleLine
    pure $ NonEmpty.map SingleLineComments $ groupComments (l :| ls)
  where
    groupComments :: ListOfSingleLineComments -> NonEmpty ListOfSingleLineComments
    groupComments ls = case NonEmpty.nonEmpty remaining of
        Nothing -> g :| []
        Just l -> g <| groupComments l
      where
        lineNumber = Megaparsec.Pos.unPos . Megaparsec.Pos.sourceLine

        (g, remaining) = removeSubseq ls
        removeSubseq :: ListOfSingleLineComments -> (ListOfSingleLineComments, [(SourcePos, Text)])
        removeSubseq (x :| []) = (x :| [], [])
        removeSubseq (x@(xPos, _) :| ys@(y@(yPos, _) : rest))
            | lineNumber yPos - lineNumber xPos == 1
                = let (subSeq, r) = removeSubseq (y :| rest) in (x <| subSeq, r)
            | otherwise = (x :| [], ys)

    singleLine = do
      sourcePos <- Text.Megaparsec.getSourcePos
      (_, commentLine) <- Token.lineComment
      whitespace
      pure (sourcePos, commentLine)

-- | Consume whitespace lines or lines that only have whitespaces *before* a comment
whitespace :: Parser ()
whitespace = Text.Megaparsec.skipMany (Text.Megaparsec.choice
    [ void (Text.Megaparsec.takeWhile1P Nothing predicate)
    , void (Token.text "\r\n")
    ] <?> "whitespace")
  where
    predicate c = c == ' ' || c == '\t' || c == '\n'

blockCommentParser :: Parser (DhallComment 'RawComment)
blockCommentParser = do
    (_, c) <- Token.blockComment
    whitespace
    pure $ BlockComment c

-- | Parse all comments in a text fragment
parseComments :: String -> Text -> [DhallComment 'RawComment]
parseComments delta text = case result of
    Left err -> error ("An error has occurred while parsing comments:\n "
      <> Text.Megaparsec.errorBundlePretty err)
    Right comments -> comments
  where
    parser = do
        comments <- many $ do
            whitespace
            lineCommentParser <|> ((:| []) <$> blockCommentParser)
        Text.Megaparsec.eof
        pure $ concatMap NonEmpty.toList comments

    result = runParser parser UnsupportedCommentsPermitted delta text

data CommentParseError
    = MissingNewlineOnBlockComment
    | BadSingleLineCommentsAlignment
    | BadPrefixesOnSingleLineComments
    | SeveralSubseqDhallDocsComments
    deriving Show

-- | Checks if a 'RawComment' has the @dhall-docs@ marker
parseMarkedComment :: DhallComment 'RawComment -> Maybe (DhallComment 'MarkedComment)
parseMarkedComment (BlockComment comment)
    | "{-|" `Data.Text.isPrefixOf` comment = Just $ BlockComment comment
    | otherwise = Nothing

parseMarkedComment (SingleLineComments ls)
    | any (("--|" `Data.Text.isPrefixOf`) . snd) ls = Just (SingleLineComments ls)
    | otherwise = Nothing

-- | Knowing that there is a @dhall-docs@ marker inside the comment, this
--   checks if a 'MarkedComment' is a 'DhallDocsComment'. For 'SingleLineComments'
--   this also removes the prefix lines before the first marked comment
parseDhallDocsComment :: DhallComment 'MarkedComment -> Either CommentParseError (DhallComment 'DhallDocsComment)
parseDhallDocsComment (BlockComment comment) =
    if any (`Data.Text.isPrefixOf` comment) ["{-|\n", "{-|\r\n"] then Right $ BlockComment comment
    else Left MissingNewlineOnBlockComment

parseDhallDocsComment (SingleLineComments lineComments) =
    fmap SingleLineComments $ checkAlignment lineComments >>= checkAmountOfMarkers >>= checkPrefixes
  where
    sourceCol = Text.Megaparsec.unPos . Text.Megaparsec.sourceColumn

    checkAmountOfMarkers :: ListOfSingleLineComments -> Either CommentParseError ListOfSingleLineComments
    checkAmountOfMarkers ls =
        if numberOfMarkers > 1 then Left SeveralSubseqDhallDocsComments
        else case newLines of
            [] -> fileAnIssue "checkAmountOfMarkers failed with newLines = []"
            l : remainder -> Right $ l :| remainder
      where
        commentLines = NonEmpty.toList ls
        numberOfMarkers = length $ filter (Data.Text.isPrefixOf "--|" . snd) commentLines
        (_, newLines) = break (Data.Text.isPrefixOf "--|" . snd) commentLines

    checkAlignment :: ListOfSingleLineComments -> Either CommentParseError ListOfSingleLineComments
    checkAlignment ls@((first, _) :| rest)
        | all ((== sourceCol first) . sourceCol . fst) rest = Right ls
        | otherwise = Left BadSingleLineCommentsAlignment

    checkPrefixes :: ListOfSingleLineComments -> Either CommentParseError ListOfSingleLineComments
    checkPrefixes ls@((_, first) :| rest)
        | "--| " `Data.Text.isPrefixOf` first && all (p . snd) rest
            = Right ls
        | otherwise = Left BadPrefixesOnSingleLineComments
      where
        p t = Data.Text.isPrefixOf "--  " t || (Data.Text.compareLength t 2 == EQ && "--" == t)

parseDhallDocsText :: DhallComment 'DhallDocsComment -> DhallDocsText
parseDhallDocsText (BlockComment blockComment) =
    case Data.Text.stripSuffix "-}" joinedText of
        Nothing -> fileAnIssue ("Obtained 'Nothing' on extractText.stripSuffix with text: \"" <> joinedText <> "\"")
        Just e -> DhallDocsText e
  where
    joinedText = Data.Text.strip $ Data.Text.unlines reIndentedLines
    (_ : commentLines) = Data.Text.lines blockComment

    leadingSpaces = Data.Text.takeWhile isSpace
        where
        isSpace t = t == ' ' || t == '\t'

    nonEmptyCommentLines = filter (not . Data.Text.null) commentLines

    commonIndentation = Data.Text.length $
        case map leadingSpaces nonEmptyCommentLines of
            l : ls -> Data.Foldable.foldl' sharedPrefix l ls
            []     -> ""
        where
        sharedPrefix ab ac =
            case Data.Text.commonPrefixes ab ac of
                Just (a, _, _) -> a
                Nothing          -> ""

    reIndentedLines = map (Data.Text.drop commonIndentation) commentLines

parseDhallDocsText (SingleLineComments (fmap snd -> (first :| rest))) =
    DhallDocsText $ Data.Text.unlines $ firstLine : map cleanRest rest
  where
    debugLines = Data.Text.unlines (first : rest)
    firstLine = case Data.Text.stripPrefix "--| " first of
        Nothing -> fileAnIssue $
            "Error strippping \"--| \" prefix on parseDhallDocsText. " <>
            "All comment lines are here:\n" <> debugLines

        Just s -> s

    cleanRest l = case Data.Text.stripPrefix "--  " l <|> Data.Text.stripPrefix "--" l of
        Nothing -> fileAnIssue $
            "Error strippping \"--  \" prefix on parseDhallDocsText. " <>
            "All comment lines are here:\n" <> debugLines

        Just s -> s

-- | Returns 'Nothing' when 'DhallDocsComment' was parsed or no error was detected
parseSingleDhallDocsComment :: String -> Text -> Maybe (Either [CommentParseError] DhallDocsText)
parseSingleDhallDocsComment delta text = do
    let rawComments = parseComments delta text
    let markedComments = Maybe.mapMaybe parseMarkedComment rawComments
    let (errors_, dhallDocsComments) = Data.Either.partitionEithers $ map parseDhallDocsComment markedComments

    let errors =
                if length dhallDocsComments >= 2 then
                    SeveralSubseqDhallDocsComments : errors_
                else
                    errors_

    case (errors, dhallDocsComments) of
        ([] ,  []) -> Nothing
        (_:_,   _) -> Just $ Left errors
        (_  , [a]) -> Just $ Right $ parseDhallDocsText a
        (_  ,   _) -> fileAnIssue "Returned more than one comment at parseSingleDhallDocsComment"
