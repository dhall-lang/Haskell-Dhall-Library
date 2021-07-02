{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

{-| Contains the logic to render the source code inside a HTML. It also provides
    context-sensitive features such as jump-to-definition.

    Rendering an expression consists on the following steps:

    * An 'Expr Src Import' with its parsed 'Text' is processed into
      a '[SourceCodeFragment]'
    * Each 'SourceCodeFragment' tells the 'renderSourceCodeFragment' how to
      render that function as HTML including the injected information through
      HTML data-attributes

    To render a Dhall file you should use 'renderCodeWithHyperLinks' which
    takes a 'Text' that was used to parse the 'Expr Src Import', and returns
    the generated 'Html ()' with the same structure (i.e. whitespaces)
    from the 'Text' argument.

    To render code-snippets (e.g. assertions from examples, type from source code)
    you should use 'renderCodeSnippet' which uses the output of @dhall format@
    as the 'Text' argument to call later 'renderCodeWithHyperLinks'
-}
module Dhall.Docs.CodeRenderer
    ( renderCodeWithHyperLinks
    , renderCodeSnippet
    , ExprType(..)
    ) where

import Control.Monad.Trans.Writer.Strict (Writer)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Dhall.Context                     (Context)
import Dhall.Core
    ( Binding (..)
    , Expr (..)
    , FieldSelection (..)
    , File (..)
    , FilePrefix (..)
    , FunctionBinding (..)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    , RecordField (..)
    , Scheme (..)
    , URL (..)
    , Var (..)
    )
import Dhall.Docs.Util
import Dhall.Src                         (Src (..))
import Lucid
import Text.Megaparsec.Pos               (SourcePos (..))

import qualified Control.Monad.Trans.Writer.Strict     as Writer
import qualified Data.Maybe as Maybe
import qualified Data.List
import qualified Data.Set                              as Set
import qualified Data.Text                             as Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import qualified Dhall.Context                         as Context
import qualified Dhall.Core                            as Core
import qualified Dhall.Map                             as Map
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Lens.Family                           as Lens
import qualified Text.Megaparsec.Pos                   as SourcePos

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Dhall.Core (Directory (..))

-- | Get the source line and column from a 'SourcePos' as an 'Int'
getSourceLine, getSourceColumn :: SourcePos -> Int
getSourceLine = SourcePos.unPos . SourcePos.sourceLine
getSourceColumn = SourcePos.unPos . SourcePos.sourceColumn

{-| Every 'Expr' constructor has extra information that tell us what to highlight on
    hover and where to jump on click events. 'JtdInfo' record that extra
    information.
-}
data JtdInfo
    {-| Each field in a Dhall record (type or literal) is associated with a
        'NameDecl', and selector-expressions behave like 'Var's by using a
        'NameUse' with the field 'NameDecl' to jump to that label.

        For example, a Dhall expression like this:

        > { a = foo, b = bar }

        has the following 'JtdInfo':

        > RecordFields (Set.fromList [NameDecl posA "a" jtdInfoA, NameDecl posB "b" jtdInfoB])

        ... where

        * @posA@ and @posB@ record the source position used to make them
        unique across the rendered source code
        * @jtdInfoA@ and @jtdInfoB@ are the associated 'JtdInfo' inferred from
        @foo@ and @bar@
    -}
    = RecordFields (Set.Set NameDecl)
    -- | Default type for cases we don't handle
    | NoInfo
    deriving (Eq, Ord, Show)

{-| To make each name unique we record the source position where it was
    found.

    The names that we handle are the ones introduced by let-bindings, lambda
    arguments and record (types and literals) labels.
-}
data NameDecl = NameDecl Src Text JtdInfo
    deriving (Eq, Ord, Show)

makeHtmlId :: NameDecl -> Text
makeHtmlId (NameDecl Src{srcStart} _ _) =
       "var"
    <> Text.pack (show $ getSourceLine srcStart) <> "-"
    <> Text.pack (show $ getSourceColumn srcStart)

-- | Available ways of rendering source code as HTML
data SourceCodeType
    -- | Relative and remote imports are rendered using an HTML anchor tag.
    --   Other imports are rendered as plain-text
    = ImportExpr Import

    -- | Used to render a name declared in let-binding or function argument
    --   that is used in any expression
    | NameUse NameDecl

    -- | Used to render the declaration of a name. This is used to jump
    --   to that name after clicking an 'NameUse'
    | NameDeclaration NameDecl

{-| The 'Expr Src Import' parsed from a 'Text' is split into a
    '[SourceCodeFragment]'.
-}
data SourceCodeFragment =
    SourceCodeFragment
        Src -- ^ The start and end position of this fragment
        SourceCodeType -- ^ The type of 'SourceCodeFragment' that will guide HTML rendering

-- | Returns all 'SourceCodeFragment's in lexicographic order i.e. in the same
--   order as in the source code.
fragments :: Expr Src Import -> [SourceCodeFragment]
fragments = Data.List.sortBy sorter . removeUnusedDecls . Writer.execWriter . infer Context.empty
  where
    sorter (SourceCodeFragment Src{srcStart = srcStart0} _)
           (SourceCodeFragment Src{srcStart = srcStart1} _) = pos0 `compare` pos1
      where
        pos0 = (getSourceLine srcStart0, getSourceColumn srcStart0)
        pos1 = (getSourceLine srcStart1, getSourceColumn srcStart1)

    removeUnusedDecls sourceCodeFragments = filter isUsed sourceCodeFragments
      where
        makePosPair Src{srcStart} = (getSourceLine srcStart, getSourceColumn srcStart)
        nameUsePos (SourceCodeFragment _ (NameUse (NameDecl src _ _))) =
            Just $ makePosPair src
        nameUsePos _ = Nothing

        usedNames = Set.fromList $ Maybe.mapMaybe nameUsePos sourceCodeFragments

        isUsed (SourceCodeFragment _ (NameDeclaration (NameDecl src _ _))) =
            makePosPair src `Set.member` usedNames
        isUsed _ = True

    infer :: Context NameDecl -> Expr Src Import -> Writer [SourceCodeFragment] JtdInfo
    infer context = \case
        -- The parsed text of the import is located in it's `Note` constructor
        Note src (Embed a) -> Writer.tell [SourceCodeFragment src $ ImportExpr a] >> return NoInfo

        -- since we have to 'infer' the 'JtdInfo' of the annotation, we
        -- are not able to generate the 'SourceCodeFragment's in lexicographical
        -- without calling 'Data.List.sortBy' after
        Let (Binding
                _
                name
                (Just Src { srcStart, srcEnd })
                _
                annotation
                _
                value) expr' -> do

            -- If annotation is missing, the type is inferred from the bound value
            case annotation of
                Nothing -> return ()
                Just (_, t) -> do
                    _ <- infer context t
                    return ()

            bindingJtdInfo <- infer context value

            let nameSrc = makeSrcForLabel srcStart srcEnd name
            let nameDecl = NameDecl nameSrc name bindingJtdInfo

            Writer.tell [SourceCodeFragment nameSrc (NameDeclaration nameDecl)]
            infer (Context.insert name nameDecl context) expr'

        Note src (Var (V name index)) ->
            case Context.lookup name index context of
                Nothing -> return NoInfo
                Just nameDecl@(NameDecl _ _ t) -> do
                    Writer.tell [SourceCodeFragment src $ NameUse nameDecl]
                    return t

        Lam _ (FunctionBinding
                _
                name
                (Just Src{srcStart, srcEnd})
                _
                _
                t) expr -> do
            dhallType <- infer context t

            let nameSrc = makeSrcForLabel srcStart srcEnd name
            let nameDecl = NameDecl nameSrc name dhallType

            Writer.tell [SourceCodeFragment nameSrc (NameDeclaration nameDecl)]
            infer (Context.insert name nameDecl context) expr

        Field e (FieldSelection _ _ label (Just Src{srcStart, srcEnd})) -> do
            fields <- do
                dhallType <- infer context e
                case dhallType of
                    NoInfo -> return mempty
                    RecordFields s -> return $ Set.toList s

            let src = makeSrcForLabel srcStart srcEnd label
            let match (NameDecl _ l _) = l == label
            case filter match fields of
                x@(NameDecl _ _ t) : _ -> do
                    Writer.tell [SourceCodeFragment src (NameUse x)]
                    return t
                _ -> return NoInfo

        RecordLit (Map.toList -> l) -> handleRecordLike l

        Record (Map.toList -> l) -> handleRecordLike l

        Note _ e -> infer context e
        e -> do
            mapM_ (infer context) $ Lens.toListOf Core.subExpressions e
            return NoInfo

      where
        handleRecordLike l = RecordFields . Set.fromList <$> mapM f l
          where
            f (key, RecordField _ (Just Src{ srcStart, srcEnd }) val _ _) = do
                dhallType <- infer context val
                let nameSrc = makeSrcForLabel srcStart srcEnd key
                let nameDecl = NameDecl nameSrc key dhallType
                Writer.tell [SourceCodeFragment nameSrc (NameDeclaration nameDecl)]
                return nameDecl
              where
            f _ = fileAnIssue "A `RecordField` of type `Expr Src Import` doesn't have `Just src*`"

fileAsText :: File -> Text
fileAsText File{..} = foldr (\d acc -> acc <> "/" <> d) "" (Core.components directory)
    <> "/" <> file

-- | Generic way of creating a Src for a label, taking quoted names into
--   account
makeSrcForLabel
    :: SourcePos  -- ^ Prefix whitespace end position, will be 'srcStart'
    -> SourcePos  -- ^ Suffix whitespace start position, will be 'srcEnd'
    -> Text       -- ^ Label name, will be the 'srcText' with surrounding @`@ if needed
    -> Src
makeSrcForLabel srcStart srcEnd name = Src {..}
  where
    realLength = getSourceColumn srcEnd - getSourceColumn srcStart
    srcText =
        if Text.length name == realLength then name
        else "`" <> name <> "`"

renderSourceCodeFragment :: SourceCodeFragment -> Html ()
renderSourceCodeFragment (SourceCodeFragment Src{..} (ImportExpr import_)) =
    renderImport import_ srcText
  where
    {-  Given an 'Import', render the contents in an HTML element that will allow
        users to jump to another file or domain. The 'Text' argument is the contents
        inside the anchor tag

        Example:

        >>> :set -Wno-missing-fields
        >>> let file = File { directory = Directory [], file = ""}
        >>> let url = URL { scheme = HTTPS, authority = "google.com", query = Nothing, path = file}
        >>> let import_ = Import {importHashed = ImportHashed { importType = Remote url }}
        >>> renderImport import_ "link for google"
        <a href="https://google.com/" target="_blank">link for google</a>
    -}
    renderImport :: Import -> Text -> Html ()
    renderImport (Import {importHashed = ImportHashed { importType }}) =
        case importType of
            Remote URL {..} -> a_ [href_ href, target_ "_blank"] . toHtml
              where
                scheme_ = case scheme of
                    HTTP -> "http"
                    HTTPS -> "https"

                path_ = fileAsText path

                query_ = case query of
                    Nothing -> ""
                    Just d -> "?" <> d

                -- we don't include the headers here since we treat links to open a file
                -- in another tab
                href = scheme_ <> "://" <> authority <> path_ <> query_

            Local Here file -> a_ [href_ href] . toHtml
              where
                href = "." <> fileAsText file <> ".html"

            Local Parent file -> a_ [href_ href] . toHtml
              where
                href = ".." <> fileAsText file <> ".html"

            _ -> toHtml

renderSourceCodeFragment (SourceCodeFragment Src{..} (NameDeclaration nameDecl)) =
    span_ attributes $ toHtml srcText
  where
    attributes =
        [id_ $ makeHtmlId nameDecl
        , class_ "name-decl"
        , data_ "name" $ makeHtmlId nameDecl ]
renderSourceCodeFragment (SourceCodeFragment Src{..} (NameUse nameDecl)) =
    a_ attributes $ toHtml srcText
  where
    attributes =
        [ href_ $ "#" <> makeHtmlId nameDecl
        , class_ "name-use"
        , data_ "name" $ makeHtmlId nameDecl
        ]

-- | Given a Text and the parsed `Expr Src Import` from it, this will render the
--   the source code on HTML with jump-to-definition on URL imports. Use this
--   to render the source code with the same structure (whitespaces, comments,
--   language elements) as the source file
renderCodeWithHyperLinks :: Text -> Expr Src Import -> Html ()
renderCodeWithHyperLinks contents expr = pre_ $ go (1, 1) lines_ imports
  where
    imports = fragments expr

    lines_ = map fixWindows (Text.lines contents)

    fixWindows line
        | Text.null line         = line
        | Text.last line == '\r' = Text.init line
        | otherwise              = line

    -- we keep the current line, column and consumed text as part of function argument
    go :: (Int, Int) -> [Text] -> [SourceCodeFragment] -> Html ()
    go _ textLines [] = mapM_ (\t -> toHtml t >> br_ []) textLines

    -- consume lines until we encounter the first 'SourceCodeFragment'
    go (currLineNumber, _) (currLine : restLines) scfs@((SourceCodeFragment Src {..} _) : _)
        | getSourceLine srcStart /= currLineNumber = do
            toHtml currLine
            br_ []
            go (currLineNumber + 1, 1) restLines scfs

    go (_, currCol) currentLines (scf@(SourceCodeFragment Src {..} _) : rest) = do
        let importStartLine = getSourceLine srcStart
        let importEndLine = getSourceLine srcEnd

        let importStartCol = getSourceColumn srcStart
        let importEndCol = getSourceColumn srcEnd

        let (importLines, suffixLines) = splitAt (importEndLine - importStartLine + 1) currentLines

        -- calls to `head` and `last` here should never fail since `importLines`
        -- have at least one element
        let (firstImportLine, lastImportLine) = (head importLines, last importLines)
        let prefixCols = Text.take (importStartCol - currCol) firstImportLine
        let suffixCols = Text.drop (importEndCol - currCol) lastImportLine

        -- render the prefix column
        toHtml prefixCols

        -- rendered element
        renderSourceCodeFragment scf

        -- add a newline if last line of import consumes the remaining line on
        -- the original text
        if Text.null suffixCols then br_ [] else return ()

        let suffix = if Text.null suffixCols then suffixLines else suffixCols : suffixLines

        -- move the cursor to next line if no characterse are remaining on the
        -- suffix cols, otherwise keep the last line and next char right after
        -- the import. This is done to handle properly several imports on the
        -- same line
        let nextPosition = if Text.null suffixCols then
                               (importEndLine + 1, 1)
                           else (importEndLine, importEndCol)

        go nextPosition suffix rest

-- | Internal utility to differentiate if a Dhall expr is a type annotation
--   or the whole file
data ExprType = TypeAnnotation | AssertionExample

-- | Renders an AST /fragment/ from the source file AST. Use this when you don't
--   have access to the 'Text' that was used to generate the AST.
--   The difference between this and 'renderCodeWithHyperLinks' is that
--   the extracted fragment's 'SourcePos's need to be re-generated to
--   render them in a better way; just adding whitespace at the beginning of the
--   first line won't render good results.
renderCodeSnippet :: Dhall.Pretty.CharacterSet -> ExprType -> Expr Void Import -> Html ()
renderCodeSnippet characterSet exprType expr = renderCodeWithHyperLinks formattedFile expr'
  where
    layout = case exprType of
        AssertionExample -> Dhall.Pretty.layout
        TypeAnnotation -> typeLayout

    formattedFile = Pretty.Text.renderStrict
        $ layout
        $ Dhall.Pretty.prettyCharacterSet characterSet (Core.denote expr)

    expr' = case Dhall.Parser.exprFromText "" formattedFile of
        Right e -> e
        Left _ -> fileAnIssue "A failure has occurred while parsing a formatted file"

    typeLayout = Pretty.removeTrailingWhitespace . Pretty.layoutSmart opts
      where
        -- this is done so the type of a dhall file fits in a single line
        -- its a safe value, since types in source codes are not that large
        opts = Pretty.defaultLayoutOptions
                { Pretty.layoutPageWidth =
                    Pretty.Unbounded
                }
