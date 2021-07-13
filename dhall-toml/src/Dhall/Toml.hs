{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

-- TODO: split module into Dhall.DhallToToml and Dhall.TomlToDhall modules
module Dhall.Toml
    ( tomlToDhallMain
    , tomlToDhall
    , dhallToTomlMain
    , dhallToToml
    ) where

import Control.Monad        (foldM)
import Control.Exception    (Exception, throwIO)
import Data.Either          (rights)
import Data.Foldable        (toList, foldl')
import Data.List.NonEmpty   (NonEmpty((:|)))
import Data.Text            (Text)
import Data.Void            (Void)
import Dhall.Core           (Expr, DhallDouble(..))
import Dhall.Parser         (Src)
import Toml.Parser          (TomlParseError)
import Toml.Type.AnyValue   (AnyValue(AnyValue))
import Toml.Type.Value      (Value)
import Toml.Type.TOML       (TOML)
import Toml.Type.Key        (Piece(Piece), Key(Key, unKey))
import Toml.Type.PrefixTree (PrefixTree)
import Toml.Type.Printer    (pretty)

import qualified Data.Bifunctor       as Bifunctor
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Sequence        as Seq
import qualified Data.Text
import qualified Data.Text.IO         as Text.IO
import qualified Dhall.Map            as Map
import qualified Dhall.Core           as Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Toml.Parser
import qualified Toml.Type.TOML       as Toml.TOML
import qualified Toml.Type.PrefixTree as Toml.PrefixTree
import qualified Toml.Type.Value      as Toml.Value
import qualified Toml.Type.Value      as Value
import qualified Toml.Type.AnyValue   as Toml.AnyValue
import qualified System.Environment


data CompileError
    = Unimplemented String
    | Unsupported (Expr Void Void)
    | Incompatible (Expr Src Void) Object
    -- | tomland does not support records in multi-dimensional arrays, though it
    --   is allowed by the spec
    | UnsupportedArray (Expr Void Void)
    | NotARecord (Expr Void Void)
    -- | the latest TOML spec, v1.0.0 allows this but tomland has not
    --   implemented it yet
    --   NOTE: the only way to get this error is through unions
    | HeterogeneousArray (Expr Void Void)
    | InvalidToml TomlParseError
    | InternalError String
    | MissingKey String

instance Show CompileError where
    show (Unimplemented s) = "unimplemented: " ++ s
    show (Unsupported e) = "unsupported: " ++ show e
    show (Incompatible e toml) = "incompatible: " ++ (show e) ++ " with " ++ (show toml)
    show (UnsupportedArray e) = "records cannot be nested in multi-dimentional arrays: " ++ show e
    show (NotARecord e) = "The root object converted to TOML must be a record, got " ++ show e
    show (HeterogeneousArray e) = "heterogeneous arrays are not currently supported " ++ show e
    show (InvalidToml e) = "invalid TOML: " ++ show e
    show (InternalError e) = "internal error: " ++ show e
    show (MissingKey e) = "missing key: " ++ show e

instance Exception CompileError

dhallToToml :: Expr s Void -> Either CompileError TOML
dhallToToml e0 = do
    r <- assertRecordLit (Core.normalize e0)
    toTomlTable r

assertRecordLit :: Expr Void Void -> Either CompileError (Map.Map Text (Core.RecordField Void Void))
assertRecordLit (Core.RecordLit r) = Right r
assertRecordLit e                  = Left $ NotARecord e

toTomlTable :: Map.Map Text (Core.RecordField Void Void) -> Either CompileError TOML
toTomlTable r = foldM (toTomlRecordFold []) (mempty :: TOML) (Map.toList r)

toTomlRecordFold :: [Piece] -> TOML -> (Text, Core.RecordField Void Void) -> Either CompileError TOML
toTomlRecordFold curKey toml' (key', val) = toToml toml' newKey (Core.recordFieldValue val)
    where
        append :: [Piece] -> Piece -> NonEmpty Piece
        append []     y = y :| []
        append (x:xs) y = x :| xs ++ [y]
        newKey = Key $ append curKey $ Piece key'

toToml :: TOML -> Key -> Expr Void Void -> Either CompileError TOML
toToml toml key expr  = case expr of
    Core.BoolLit a -> return $ insertPrim (Toml.Value.Bool a)
    Core.NaturalLit a -> return $ insertPrim (Toml.Value.Integer $ toInteger a)
    Core.DoubleLit (DhallDouble a) -> return $ insertPrim (Toml.Value.Double a)
    Core.TextLit (Core.Chunks [] a) -> return $ insertPrim (Toml.Value.Text a)
    -- empty union alternative like < A | B >.A
    Core.Field (Core.Union _) (Core.FieldSelection _ a _) -> return $ insertPrim (Toml.Value.Text a)
    -- union alternative with type like < A : Natural | B>.A 1
    Core.App (Core.Field (Core.Union _) _) a -> toToml toml key a
    Core.ListLit _ a -> case toList a of
        -- empty array
        [] -> return $ insertPrim (Toml.Value.Array [])
        -- array of table
        record@(Core.RecordLit _) : records -> do
            tables' <- case mapM assertRecordLit (record :| records)  of
                Right x -> mapM toTomlTable x
                Left (NotARecord e) -> Left (HeterogeneousArray e)
                Left x -> Left x
            return $ Toml.TOML.insertTableArrays key tables' toml
        -- inline array
        a' -> do
            anyList <- mapM toAny a'
            let arrayEither = Toml.AnyValue.toMArray anyList
            array <- Bifunctor.first (const $ HeterogeneousArray expr) arrayEither
            return $ insertPrim array
    Core.RecordLit r ->
        let
            (inline, nested) = Map.partition (isInline . Core.recordFieldValue) r
        in
            if null inline
            -- if the table doesn't have inline elements, don't register
            -- the table, only its non-inlined children. Ex:
            -- [a] # bad
            --   [b]
            --     c = 1
            -- [a.b] # good
            --   c = 1
            then foldM (toTomlRecordFold $ toList $ unKey key) toml (Map.toList nested)
            else do
                -- the order here is important, at least for testing, because
                -- the PrefixMap inside TOML is dependent on insert order
                inlinePairs <- foldM (toTomlRecordFold []) mempty      (Map.toList inline)
                nestedPairs <- foldM (toTomlRecordFold []) inlinePairs (Map.toList nested)
                return $ Toml.TOML.insertTable key nestedPairs toml
    _ -> Left $ Unsupported expr
    where
        insertPrim :: Toml.Value.Value a -> TOML
        insertPrim val = Toml.TOML.insertKeyVal key val toml

        -- checks if the value should be represented as an inline key/value
        -- pair. Elements that are inlined are those that do not have a
        -- [header] or [[header]]. One edge case is tables within multiple
        -- arrays, though not currently supported by tomland, can only
        -- be represented as inline tables.
        isInline v = case v of
            Core.BoolLit _    -> True
            Core.NaturalLit _ -> True
            Core.DoubleLit _  -> True
            Core.TextLit _    -> True
            Core.ListLit _ s  -> case Seq.lookup 0 s of
                Nothing                  -> True
                Just (Core.BoolLit _)    -> True
                Just (Core.NaturalLit _) -> True
                Just (Core.DoubleLit _)  -> True
                Just (Core.TextLit _)    -> True
                Just (Core.ListLit _ _)  -> True
                _                        -> False
            _ -> False

        rightAny = Right . Toml.AnyValue.AnyValue

        -- toAny is a helper function for making lists so it returns a list
        -- specific error, in particular tomland's inability to represent
        -- tables in multi-dimensional arrays
        toAny :: Expr Void Void -> Either CompileError Toml.AnyValue.AnyValue
        toAny e = case e of
            Core.BoolLit x                  -> rightAny $ Toml.Value.Bool x
            Core.NaturalLit x               -> rightAny $ Toml.Value.Integer $ toInteger x
            Core.DoubleLit (DhallDouble x)  -> rightAny $ Toml.Value.Double x
            Core.TextLit (Core.Chunks [] x) -> rightAny $ Toml.Value.Text x
            Core.ListLit _ x                -> do
                anyList <- mapM toAny $ toList x
                case Toml.AnyValue.toMArray anyList of
                    Right x' -> rightAny x'
                    Left _ -> Left $ HeterogeneousArray expr
            Core.RecordLit _ -> Left $ UnsupportedArray e
            _ -> Left $ Unsupported e


tomlToDhall :: Expr Src Void -> TOML -> Either CompileError (Expr Src Void)
tomlToDhall schema toml = toDhall (Core.normalize schema) (tomlToObject toml)

tomlValueToDhall :: Expr Src Void -> Value t -> Either CompileError (Expr Src Void)
tomlValueToDhall exprType v = case (exprType, v) of
    (Core.Bool                , Value.Bool a   ) -> Right $ Core.BoolLit a
    (Core.Natural             , Value.Integer a) -> Right $ Core.NaturalLit $ fromInteger a
    (Core.Double              , Value.Double a ) -> Right $ Core.DoubleLit $ DhallDouble a
    (Core.Text                , Value.Text a   ) -> Right $ Core.TextLit $ Core.Chunks [] a
    (_                        , Value.Zoned _  ) -> Left $ Unimplemented "toml time values"
    (_                        , Value.Local _  ) -> Left $ Unimplemented "toml time values"
    (_                        , Value.Day _    ) -> Left $ Unimplemented "toml time values"
    (t@(Core.App Core.List _) , Value.Array [] ) -> Right $ Core.ListLit (Just t) []
    (Core.App Core.List t     , Value.Array a  ) -> do
        l <- mapM (tomlValueToDhall t) a
        return $ Core.ListLit Nothing (Seq.fromList l)
    _ -> Left $ Incompatible exprType (Prim (AnyValue v))

-- TODO: keep track of the path for more helpful error mesasges
toDhall :: Expr Src Void -> Object -> Either CompileError (Expr Src Void)
toDhall exprType value = case (exprType, value) of
    (_,                    Invalid)  -> Left $ InternalError "invalid object"

    -- TODO: allow different types of matching
    (Core.Union m        , _)        -> let
        f key maybeType = case maybeType of
            Just ty -> do
                expr <- toDhall ty value
                return $ Core.App (Core.Field exprType $ Core.makeFieldSelection key) expr
            Nothing -> case value of
                Prim (AnyValue (Value.Text a)) | a == key ->
                    return $ Core.Field exprType (Core.makeFieldSelection a)
                _ -> Left $ Incompatible exprType value

        in case rights (toList (Map.mapWithKey f m)) of
            []  -> Left $ Incompatible exprType value
            x:_ -> Right $ x

    (Core.App Core.List t, Array []) -> Right $ Core.ListLit (Just t) []

    (Core.App Core.List t, Array a) -> do
        l <- mapM (toDhall t) a
        return $ Core.ListLit Nothing (Seq.fromList l)

    (Core.Record r, Table t) -> let
        f :: Text -> (Expr Src Void) -> Either CompileError (Expr Src Void)
        f k ty | Just val <- HashMap.lookup (Piece k) t = toDhall ty val
               | Core.App Core.Optional ty' <- ty = Right $ (Core.App Core.None ty')
               | Core.App Core.List _ <- ty = Right $ Core.ListLit (Just ty) []
               | otherwise = Left $ MissingKey $ Data.Text.unpack k
        in do
            values <- Map.traverseWithKey f (Core.recordFieldValue <$> r)
            return $ Core.RecordLit (Core.makeRecordField <$> values)

    (_, Prim (AnyValue v)) -> tomlValueToDhall exprType v

    (ty, obj) -> Left $ Incompatible ty obj


-- | An intermediate object created from a 'TOML' before an 'Expr'.
-- | It does two things, firstly joining the tomlPairs, tomlTables,
-- | and tomlTableArrays parts of the TOML. Second, it turns the dense
-- | paths (ex. a.b.c = 1) into sparse paths (ex. a = { b = { c = 1 }}).
data Object
    = Prim Toml.AnyValue.AnyValue
    | Array [Object]
    | Table (HashMap.HashMap Piece Object)
    | Invalid
    deriving (Show)

instance Semigroup Object where
    (Table ls) <> (Table rs) = Table (ls <> rs)
    -- this shouldn't happen because tomland has already verified correctness
    -- of the toml object
    _ <> _ = Invalid

-- | Creates an arbitrarily nested object
sparseObject :: Key -> Object -> Object
sparseObject (Key (piece :| [])) value = Table $ HashMap.singleton piece value
sparseObject (Key (piece :| rest:rest')) value
    = Table $ HashMap.singleton piece (sparseObject (Key $ rest :| rest') value)

pairsToObject :: HashMap.HashMap Key Toml.AnyValue.AnyValue -> Object
pairsToObject pairs
    = foldl' (<>) (Table HashMap.empty)
    $ HashMap.mapWithKey sparseObject
    $ (fmap Prim pairs)

tablesToObject :: Toml.PrefixTree.PrefixMap TOML -> Object
tablesToObject tables
    = foldl' (<>) (Table HashMap.empty)
    $ map prefixTreeToObject
    $ HashMap.elems tables

prefixTreeToObject :: PrefixTree TOML -> Object
prefixTreeToObject (Toml.PrefixTree.Leaf key toml)
    = sparseObject key (tomlToObject toml)
prefixTreeToObject (Toml.PrefixTree.Branch prefix _ toml)
    = sparseObject prefix (tablesToObject toml)

tableArraysToObject :: HashMap.HashMap Key (NonEmpty TOML) -> Object
tableArraysToObject arrays
    = foldl' (<>) (Table HashMap.empty)
    $ HashMap.mapWithKey sparseObject
    $ fmap (Array . fmap tomlToObject . toList)  arrays

tomlToObject :: TOML -> Object
tomlToObject toml = pairs <> tables <> tableArrays
    where
        pairs = pairsToObject $ Toml.TOML.tomlPairs toml
        tables = tablesToObject $ Toml.TOML.tomlTables toml
        tableArrays = tableArraysToObject $ Toml.TOML.tomlTableArrays toml

dhallToTomlMain :: IO ()
dhallToTomlMain = do
    text <- Text.IO.getContents
    parsedExpression <- Core.throws (Dhall.Parser.exprFromText "(input)" text)
    resolvedExpression <- Dhall.Import.load parsedExpression
    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)
    toml <- case dhallToToml resolvedExpression of
        Left err -> throwIO err
        Right toml -> return toml
    Text.IO.putStrLn $ pretty toml


tomlToDhallMain :: IO ()
tomlToDhallMain = do
    text <- Text.IO.getContents
    toml <- case Toml.Parser.parse text of
        Left tomlErr -> throwIO (InvalidToml tomlErr)
        Right toml -> return toml
    args <- System.Environment.getArgs
    schemaFile <- case args of
        [] -> fail "schema not provided"
        schemaFile:[] -> return schemaFile
        _ -> fail "too many agrgs"
    schema <- fileToDhall schemaFile
    dhall <- case tomlToDhall schema toml of
        Left err -> throwIO err
        Right dhall -> return dhall
    Text.IO.putStrLn $ Core.pretty dhall

fileToDhall :: String -> IO (Core.Expr Src Void)
fileToDhall file = do
    text <- Text.IO.readFile file
    parsedExpression <-
        Core.throws (Dhall.Parser.exprFromText file text)
    resolvedExpression <- Dhall.Import.load parsedExpression
    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)
    return resolvedExpression

