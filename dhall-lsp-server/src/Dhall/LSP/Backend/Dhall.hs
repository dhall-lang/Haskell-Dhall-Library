module Dhall.LSP.Backend.Dhall (
  FileIdentifier,
  fileIdentifierFromFilePath,
  fileIdentifierFromURI,
  hashNormalToCode,
  WellTyped,
  fromWellTyped,
  Normal,
  fromNormal,
  Cache,
  emptyCache,
  invalidate,
  DhallError(..),
  parse,
  parseWithHeader,
  load,
  typecheck,
  normalize
 ) where

import Dhall.Core   (Expr)
import Dhall.Parser (Src)

import Control.Exception                (SomeException, catch)
import Control.Lens                     (set, view)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Bifunctor                   (first)
import Data.List.NonEmpty               (NonEmpty ((:|)))
import Data.Text                        (Text)
import Data.Void                        (Void)
import Network.URI                      (URI)
import System.FilePath
    ( splitDirectories
    , takeDirectory
    , takeFileName
    )

import qualified Data.Graph                 as Graph
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Dhall.Core                 as Dhall
import qualified Dhall.Import               as Dhall
import qualified Dhall.Map
import qualified Dhall.Parser               as Dhall
import qualified Dhall.TypeCheck            as Dhall
import qualified Language.Haskell.LSP.Types as LSP.Types
import qualified Network.URI                as URI


-- | A @FileIdentifier@ represents either a local file or a remote url.
newtype FileIdentifier = FileIdentifier Dhall.Chained

-- | Construct a FileIdentifier from a local file path.
fileIdentifierFromFilePath :: FilePath -> FileIdentifier
fileIdentifierFromFilePath path =
  let filename = Text.pack $ takeFileName path
      directory = takeDirectory path
      components = map Text.pack . reverse . splitDirectories $ directory
      file = Dhall.File (Dhall.Directory components) filename
  in FileIdentifier $ Dhall.chainedFromLocalHere Dhall.Absolute file Dhall.Code

-- | Construct a FileIdentifier from a given URI. Supports only "file:" URIs.
fileIdentifierFromURI :: URI -> Maybe FileIdentifier
fileIdentifierFromURI uri
  | URI.uriScheme uri == "file:" = do
    path <- LSP.Types.uriToFilePath . LSP.Types.Uri . Text.pack
                  $ URI.uriToString id uri ""
    return $ fileIdentifierFromFilePath path
fileIdentifierFromURI _ = Nothing

-- | A well-typed expression.
newtype WellTyped = WellTyped {fromWellTyped :: Expr Src Void}

-- | A fully normalised expression.
newtype Normal = Normal {fromNormal :: Expr Src Void}

-- An import graph, represented by list of import dependencies.
type ImportGraph = [Dhall.Depends]

-- | A cache maps Dhall imports to fully normalised expressions. By reusing
--   caches we can speeds up diagnostics etc. significantly!
data Cache = Cache ImportGraph (Dhall.Map.Map Dhall.Chained Dhall.ImportSemantics)

-- | The initial cache.
emptyCache :: Cache
emptyCache = Cache [] Dhall.Map.empty

-- | Invalidate any _unhashed_ imports of the given file. Hashed imports are
--   kept around as per
--   https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md.
--   Transitively invalidates any imports depending on the changed file.
invalidate :: FileIdentifier -> Cache -> Cache
invalidate (FileIdentifier chained) (Cache dependencies cache) =
  Cache dependencies' $ Dhall.Map.withoutKeys cache invalidImports
  where
    imports = map Dhall.parent dependencies ++ map Dhall.child dependencies

    adjacencyLists = foldr
                       -- add reversed edges to adjacency lists
                       (\(Dhall.Depends parent child) -> Map.adjust (parent :) child)
                       -- starting from the discrete graph
                       (Map.fromList [ (i,[]) | i <- imports])
                       dependencies

    (graph, importFromVertex, vertexFromImport) = Graph.graphFromEdges
      [(node, node, neighbours) | (node, neighbours) <- Map.assocs adjacencyLists]

    -- compute the reverse dependencies, i.e. the imports reachable in the transposed graph
    reachableImports import_ =
      (map ((\ (i, _, _) -> i) . importFromVertex) . concat) $
        do vertex <- vertexFromImport import_
           return (Graph.reachable graph vertex)

    codeImport = Dhall.chainedChangeMode Dhall.Code chained
    textImport = Dhall.chainedChangeMode Dhall.RawText chained
    invalidImports = Set.fromList $ codeImport : reachableImports codeImport
                                    ++ textImport : reachableImports textImport

    dependencies' = filter (\(Dhall.Depends parent child) -> Set.notMember parent invalidImports
                                && Set.notMember child invalidImports) dependencies

-- | A Dhall error. Covers parsing, resolving of imports, typechecking and
--   normalisation.
data DhallError = ErrorInternal SomeException
                | ErrorImportSourced (Dhall.SourcedException Dhall.MissingImports)
                | ErrorTypecheck (Dhall.TypeError Src Void)
                | ErrorParse Dhall.ParseError

-- | Parse a Dhall expression.
parse :: Text -> Either DhallError (Expr Src Dhall.Import)
parse = fmap snd . parseWithHeader

-- | Parse a Dhall expression along with its "header", i.e. whitespace and
--   comments prefixing the actual code.
parseWithHeader :: Text -> Either DhallError (Dhall.Header, Expr Src Dhall.Import)
parseWithHeader = first ErrorParse . Dhall.exprAndHeaderFromText Dhall.UnsupportedCommentsPermitted ""

-- | Resolve all imports in an expression.
load :: FileIdentifier -> Expr Src Dhall.Import -> Cache ->
  IO (Either DhallError (Cache, Expr Src Void))
load (FileIdentifier chained) expr (Cache graph cache) = do
  let emptyStatus = Dhall.emptyStatus ""
      status = -- reuse cache and import graph
               set Dhall.cache cache .
               set Dhall.graph graph .
               -- set "root import"
               set Dhall.stack (chained :| [])
                 $ emptyStatus
  (do (expr', status') <- runStateT (Dhall.loadWith expr) status
      let cache' = view Dhall.cache status'
          graph' = view Dhall.graph status'
      return . Right $ (Cache graph' cache', expr'))
    `catch` (\e -> return . Left $ ErrorImportSourced e)
    `catch` (\e -> return . Left $ ErrorInternal e)

-- | Typecheck a fully resolved expression. Returns a certification that the
--   input was well-typed along with its (well-typed) type.
typecheck :: Expr Src Void -> Either DhallError (WellTyped, WellTyped)
typecheck expr = case Dhall.typeOf expr of
  Left err -> Left $ ErrorTypecheck err
  Right typ -> Right (WellTyped expr, WellTyped typ)

-- | Normalise a well-typed expression.
normalize :: WellTyped -> Normal
normalize (WellTyped expr) = Normal $ Dhall.normalize expr

-- | Given a normal expression compute the hash (using the default standard
--   version) of its alpha-normal form. Returns the hash in the format used in
--   Dhall's hash annotations (prefixed by "sha256:" and base-64 encoded).
hashNormalToCode :: Normal -> Text
hashNormalToCode (Normal expr) =
  Dhall.hashExpressionToCode (Dhall.denote alphaNormal)
  where alphaNormal = Dhall.alphaNormalize expr
