{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

{-| This library only exports a single `dhallToJSON` function for translating a
    Dhall syntax tree to a JSON syntax tree (i.e. a `Value`) for the @aeson@
    library

    NOTE: The @yaml@ library uses the same `Value` type to represent YAML
    files, so you can use this to convert Dhall expressions to YAML, too

    See the @dhall@ package if you would like to transform Dhall source code
    into a Dhall syntax tree.  Similarly, see the @aeson@ package if you would
    like to translate a JSON syntax tree into JSON.

    This package also provides @dhall-to-json@ and @dhall-to-yaml@ executables
    which you can use to compile Dhall source code directly to JSON or YAML for
    your convenience

    Not all Dhall expressions can be converted to JSON since JSON is not a
    programming language.  The only things you can convert are:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@ values
    * @List@s
    * @Optional@ values
    * unions
    * records

    Dhall @Bool@s translate to JSON bools:

> $ dhall-to-json <<< 'True'
> true
> $ dhall-to-json <<< 'False'
> false

    Dhall numbers translate to JSON numbers:

> $ dhall-to-json <<< '+2'
> 2
> $ dhall-to-json <<< '2'
> 2
> $ dhall-to-json <<< '2.3'
> 2.3

    Dhall @Text@ translates to JSON text:

> $ dhall-to-json <<< '"ABC"'
> "ABC"

    Dhall @List@s translate to JSON lists:

> $ dhall-to-json <<< '[1, 2, 3] : List Natural'
> [
>   1,
>   2,
>   3
> ]

    Dhall @Optional@ values translate to @null@ if absent and the unwrapped
    value otherwise:

> $ dhall-to-json <<< 'None Natural'
> null
> $ dhall-to-json <<< 'Some 1'
> 1

    Dhall records translate to JSON records:

> $ dhall-to-json <<< '{ foo = 1, bar = True }'
> {
>   "bar": true,
>   "foo": 1
> }

    Dhall unions translate to the wrapped value:

> $ dhall-to-json <<< "< Left : Natural | Right : Natural>.Left 2"
> 2
> $ cat config
> let MyType =
>       < Person : { age : Natural, name : Text } | Place : { location : Text } >
>
> in  [ MyType.Person { age = 47, name = "John" }
>     , MyType.Place { location = "North Pole" }
>     , MyType.Place { location = "Sahara Desert" }
>     , MyType.Person { age = 35, name = "Alice" }
>     ]
> $ dhall-to-json <<< "./config"
> [
>   {
>     "age": 47,
>     "name": "John"
>   },
>   {
>     "location": "North Pole"
>   },
>   {
>     "location": "Sahara Desert"
>   },
>   {
>     "age": 35,
>     "name": "Alice"
>   }
> ]

    You can preserve the name of the alternative if you wrap the value in a
    record with three fields:

    * @contents@: The union literal that you want to preserve the tag of

    * @field@: the name of the field that will store the name of the
      alternative

    * @nesting@: A value of type @\< Inline | Nested : Text \>@.

    If @nesting@ is set to @Inline@ and the union literal stored in @contents@
    contains a record then the name of the alternative is stored inline within
    the same record.  For example, this code:

> let Example = < Left : { foo : Natural } | Right : { bar : Bool } >
>
> let Nesting = < Inline | Nested : Text >
>
> in  { field    = "name"
>     , nesting  = Nesting.Inline
>     , contents = Example.Left { foo = 2 }
>     }

    ... produces this JSON:

> {
>   "foo": 2,
>   "name": "Left"
> }

    If @nesting@ is set to @Nested nestedField@ then the union is stored
    underneath a field named @nestedField@.  For example, this code:

> let Example = < Left : { foo : Natural } | Right : { bar : Bool } >
>
> let Nesting = < Inline | Nested : Text >
>
> in  { field    = "name"
>     , nesting  = Nesting.Nested "value"
>     , contents = Example.Left { foo = 2 }
>     }

    ... produces this JSON:

> {
>   "name": "Left",
>   "value": {
>     "foo": 2
>   }
> }

    You can also translate Dhall expressions encoding weakly-typed JSON
    (see: <https://prelude.dhall-lang.org/JSON/Type>):

> $ cat ./example.dhall
> let JSON = https://prelude.dhall-lang.org/JSON/package.dhall
>
> in  JSON.object
>     [ { mapKey = "foo", mapValue = JSON.null }
>     , { mapKey =
>           "bar"
>       , mapValue =
>           JSON.array [ JSON.number 1.0, JSON.bool True ]
>       }
>     ]

    By default, the fields that are evaluated to @null@ will be removed,
    but here we're preserving them with the @--preserveNull@ flag.

> $ dhall-to-json --preserveNull <<< './example.dhall'
> {
>   "bar": [
>     1,
>     true
>   ],
>   "foo": null
> }

    Also, all Dhall expressions are normalized before translation to JSON:

> $ dhall-to-json <<< "True == False"
> false

-}

module Dhall.JSON (
    -- * Dhall to JSON
      dhallToJSON
    , omitNull
    , omitEmpty
    , parsePreservationAndOmission
    , Conversion(..)
    , defaultConversion
    , convertToHomogeneousMaps
    , parseConversion
    , SpecialDoubleMode(..)
    , handleSpecialDoubles
    , codeToValue

    -- * Exceptions
    , CompileError(..)
    ) where

import Control.Applicative       (empty, (<|>))
import Control.Exception         (Exception, throwIO)
import Control.Monad             (guard)
import Data.Aeson                (ToJSON (..), Value (..))
import Data.Maybe                (fromMaybe)
import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Data.Void                 (Void)
import Dhall.Core                (Binding (..), DhallDouble (..), Expr)
import Dhall.Import              (SemanticCacheMode (..))
import Dhall.JSON.Util           (pattern FA, pattern V)
import Dhall.Map                 (Map)
import Options.Applicative       (Parser)
import Prelude                   hiding (getContents)

import qualified Data.Aeson                            as Aeson
import qualified Data.Foldable                         as Foldable
import qualified Data.HashMap.Strict                   as HashMap
import qualified Data.List
import qualified Data.Map
import qualified Data.Ord
import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Vector                           as Vector
import qualified Dhall.Core                            as Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Optics
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Dhall.Util
import qualified Lens.Family                           as Lens
import qualified Options.Applicative
import qualified System.FilePath

{-| This is the exception type for errors that might arise when translating
    Dhall to JSON

    Because the majority of Dhall language features do not translate to JSON
    this just returns the expression that failed
-}
data CompileError
    = Unsupported (Expr Void Void)
    | SpecialDouble Double
    | BareNone
    | InvalidInlineContents (Expr Void Void) (Expr Void Void)

instance Show CompileError where
    show BareNone =
       Data.Text.unpack $
            _ERROR <> ": ❰None❱ is not valid on its own                                      \n\
            \                                                                                \n\
            \Explanation: The conversion to JSON/YAML does not accept ❰None❱ in isolation as \n\
            \a valid way to represent ❰null❱.  In Dhall, ❰None❱ is a function whose input is \n\
            \a type and whose output is an ❰Optional❱ of that type.                          \n\
            \                                                                                \n\
            \For example:                                                                    \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────────────────┐  ❰None❱ is a function whose result is   \n\
            \    │ None : ∀(a : Type) → Optional a │  an ❰Optional❱ value, but the function  \n\
            \    └─────────────────────────────────┘  itself is not a valid ❰Optional❱ value \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────────────────┐  ❰None Natural❱ is a valid ❰Optional❱   \n\
            \    │ None Natural : Optional Natural │  value (an absent ❰Natural❱ number in   \n\
            \    └─────────────────────────────────┘  this case)                             \n\
            \                                                                                \n\
            \                                                                                \n\
            \                                                                                \n\
            \The conversion to JSON/YAML only translates the fully applied form to ❰null❱.   "

    show (SpecialDouble n) =
       Data.Text.unpack $
            _ERROR <> ": " <> special <> " disallowed in JSON                                \n\
            \                                                                                \n\
            \Explanation: The JSON standard does not define a canonical way to encode        \n\
            \❰NaN❱/❰Infinity❱/❰-Infinity❱.  You can fix this error by either:                \n\
            \                                                                                \n\
            \● Using ❰dhall-to-yaml❱ instead of ❰dhall-to-json❱, since YAML does support     \n\
            \  ❰NaN❱/❰Infinity❱/❰-Infinity❱                                                  \n\
            \                                                                                \n\
            \● Enabling the ❰--approximate-special-doubles❱ flag which will encode ❰NaN❱ as  \n\
            \  ❰null❱, ❰Infinity❱ as the maximum ❰Double❱, and ❰-Infinity❱ as the minimum    \n\
            \❰Double❱                                                                        \n\
            \                                                                                \n\
            \● See if there is a way to remove ❰NaN❱/❰Infinity❱/❰-Infinity❱ from the         \n\
            \  expression that you are converting to JSON                                    "
      where
        special = Data.Text.pack (show n)

    show (Unsupported e) =
        Data.Text.unpack $
            _ERROR <> ": Cannot translate to JSON                                            \n\
            \                                                                                \n\
            \Explanation: Only primitive values, records, unions, ❰List❱s, and ❰Optional❱    \n\
            \values can be translated from Dhall to JSON                                     \n\
            \                                                                                \n\
            \The following Dhall expression could not be translated to JSON:                 \n\
            \                                                                                \n\
            \" <> insert e

    show (InvalidInlineContents record alternativeContents) =
        Data.Text.unpack $
            _ERROR <> ": Union value is not compatible with ❰Inline❱ nesting.                \n\
            \                                                                                \n\
            \Explanation: You can use the ❰Inline❱ nesting to compactly encode a union while \n\
            \preserving the name of the alternative. However the alternative must either be  \n\
            \empty or contain a record value.                                                \n\
            \                                                                                \n\
            \For example:                                                                    \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────────────────────────────────┐                         \n\
            \    │ let Example = < Empty | Record : { x : Bool } > │                         \n\
            \    │                                                 │                         \n\
            \    │ let Nesting = < Inline | Nested : Text >        │                         \n\
            \    │                                                 │                         \n\
            \    │ in  { field = \"name\"                            │                       \n\
            \    │     , nesting = Nesting.Inline                  │                         \n\
            \    │     , contents = Example.Empty                  │ An empty alternative    \n\
            \    │     }                                           │ is ok.                  \n\
            \    └─────────────────────────────────────────────────┘                         \n\
            \                                                                                \n\
            \                                                                                \n\
            \... is converted to this JSON:                                                  \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────┐                                                     \n\
            \    │ { \"name\": \"Empty\" } │                                                 \n\
            \    └─────────────────────┘                                                     \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌──────────────────────────────────────────────┐                            \n\
            \    │ ...                                          │                            \n\
            \    │                                              │                            \n\
            \    │ in  { field = \"name\"                         │                          \n\
            \    │     , nesting = Nesting.Inline               │                            \n\
            \    │     , contents = Example.Record { x = True } │ An alternative containing  \n\
            \    │     }                                        │ a record value is ok.      \n\
            \    └──────────────────────────────────────────────┘                            \n\
            \                                                                                \n\
            \                                                                                \n\
            \... is converted to this JSON:                                                  \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────────────────┐                                         \n\
            \    │ { \"name\": \"Record\", \"x\": true } │                                   \n\
            \    └─────────────────────────────────┘                                         \n\
            \                                                                                \n\
            \                                                                                \n\
            \This isn't valid:                                                               \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌──────────────────────────────────────────┐                                \n\
            \    │ let Example = < Foo : Bool >             │                                \n\
            \    │                                          │                                \n\
            \    │ let Nesting = < Inline | Nested : Text > │                                \n\
            \    │                                          │                                \n\
            \    │ in  { field = \"name\"                     │                              \n\
            \    │     , nesting = Nesting.Inline           │                                \n\
            \    │     , contents = Example.Foo True        │ ❰True❱ is not a record         \n\
            \    │     }                                    │                                \n\
            \    └──────────────────────────────────────────┘                                \n\
            \                                                                                \n\
            \                                                                                \n\
            \The following Dhall expression could not be translated to JSON:                 \n\
            \                                                                                \n\
            \" <> insert record <> "                                                         \n\
            \                                                                                \n\
            \... because                                                                     \n\
            \                                                                                \n\
            \" <> insert alternativeContents <> "                                            \n\
            \                                                                                \n\
            \... is not a record."

_ERROR :: Data.Text.Text
_ERROR = Dhall.Util._ERROR

insert :: Pretty a => a -> Text
insert = Pretty.renderStrict . Dhall.Pretty.layout . Dhall.Util.insert

instance Exception CompileError

{-| Convert a Dhall expression to the equivalent JSON expression

>>> :set -XOverloadedStrings
>>> :set -XOverloadedLists
>>> import Core
>>> dhallToJSON (RecordLit [("foo", IntegerLit 1), ("bar", TextLit "ABC")])
Right (Object (fromList [("foo",Number 1.0),("bar",String "ABC")]))
>>> fmap Aeson.encode it
Right "{\"foo\":1,\"bar\":\"ABC\"}"
-}
dhallToJSON
    :: Expr s Void
    -> Either CompileError Value
dhallToJSON e0 = loop (Core.alphaNormalize (Core.normalize e0))
  where
    loop e = case e of
        Core.BoolLit a -> return (toJSON a)
        Core.NaturalLit a -> return (toJSON a)
        Core.IntegerLit a -> return (toJSON a)
        Core.DoubleLit (DhallDouble a) -> return (toJSON a)
        Core.TextLit (Core.Chunks [] a) -> return (toJSON a)
        Core.ListLit _ a -> do
            a' <- traverse loop a
            return (toJSON a')
        Core.Some a -> do
            a' <- loop a
            return (toJSON a')
        Core.App Core.None _ -> return Aeson.Null
        -- Provide a nicer error message for a common user mistake.
        --
        -- See: https://github.com/dhall-lang/dhall-lang/issues/492
        Core.None -> Left BareNone
        Core.RecordLit a ->
            case toOrderedList a of
                [   (   "contents"
                    ,   Core.recordFieldValue -> contents
                    )
                 ,  (   "field"
                    ,   Core.recordFieldValue -> Core.TextLit
                            (Core.Chunks [] field)
                    )
                 ,  (   "nesting"
                    ,   Core.recordFieldValue -> Core.App
                            (Core.Field
                                (Core.Union
                                    [ ("Inline", mInlineType)
                                    , ("Nested", Just Core.Text)
                                    ]
                                )
                                (FA "Nested")
                            )
                            (Core.TextLit
                                (Core.Chunks [] nestedField)
                            )
                    )
                 ] | all (== Core.Record []) mInlineType
                   , Just (alternativeName, mExpr) <- getContents contents -> do
                       contents' <- case mExpr of
                           Just expr -> loop expr
                           Nothing   -> return Aeson.Null

                       let taggedValue =
                               Data.Map.fromList
                                   [   (   field
                                       ,   toJSON alternativeName
                                       )
                                   ,   (   nestedField
                                       ,   contents'
                                       )
                                   ]

                       return (Aeson.toJSON taggedValue)

                [   (   "contents"
                    ,   Core.recordFieldValue -> contents
                    )
                 ,  (   "field"
                    ,   Core.recordFieldValue -> Core.TextLit
                            (Core.Chunks [] field)
                    )
                 ,  (   "nesting"
                    ,   Core.recordFieldValue -> nesting
                    )
                 ] | isInlineNesting nesting
                   , Just (alternativeName, mExpr) <- getContents contents -> do
                       kvs0 <- case mExpr of
                           Just (Core.RecordLit kvs) -> return kvs
                           Just alternativeContents ->
                               Left (InvalidInlineContents e alternativeContents)
                           Nothing -> return mempty

                       let name = Core.makeRecordField $ Core.TextLit (Core.Chunks [] alternativeName)

                       let kvs1 = Dhall.Map.insert field name kvs0

                       loop (Core.RecordLit kvs1)

                _ -> do
                    a' <- traverse (loop . Core.recordFieldValue) a
                    return (Aeson.toJSON (Dhall.Map.toMap a'))
        Core.App (Core.Field (Core.Union _) _) b -> loop b
        Core.Field (Core.Union _) (FA k) -> return (Aeson.toJSON k)
        Core.Lam _ (Core.functionBindingAnnotation -> Core.Const Core.Type)
            (Core.Lam _ (Core.functionBindingAnnotation ->
                (Core.Record
                    [ ("array" , Core.recordFieldValue -> Core.Pi _ _ (Core.App Core.List (V 0)) (V 1))
                    , ("bool"  , Core.recordFieldValue -> Core.Pi _ _ Core.Bool (V 1))
                    , ("null"  , Core.recordFieldValue -> V 0)
                    , ("number", Core.recordFieldValue -> Core.Pi _ _ Core.Double (V 1))
                    , ("object", Core.recordFieldValue ->
                        Core.Pi _ _ (Core.App Core.List (Core.Record
                        [ ("mapKey", Core.recordFieldValue -> Core.Text)
                        , ("mapValue", Core.recordFieldValue -> V 0)])) (V 1))
                    , ("string", Core.recordFieldValue -> Core.Pi _ _ Core.Text (V 1))
                    ]
                ))
                value
            ) -> do
                let outer (Core.Field (V 0) (FA "null")) = return Aeson.Null
                    outer (Core.App (Core.Field (V 0) (FA "bool")) (Core.BoolLit b)) =
                        return (Aeson.Bool b)
                    outer (Core.App (Core.Field (V 0) (FA "array")) (Core.ListLit _ xs)) = do
                        ys <- traverse outer (Foldable.toList xs)

                        return (Aeson.Array (Vector.fromList ys))
                    outer (Core.App (Core.Field (V 0) (FA "object")) (Core.ListLit _ xs)) = do
                        let inner (Core.RecordLit
                                [ ("mapKey", Core.recordFieldValue -> Core.TextLit (Core.Chunks [] mapKey))
                                , ("mapValue", Core.recordFieldValue -> mapExpression)]) = do
                                mapValue <- outer mapExpression

                                return (mapKey, mapValue)
                            inner _ = Left (Unsupported e)

                        ys <- traverse inner (Foldable.toList xs)

                        return (Aeson.Object (HashMap.fromList ys))
                    outer (Core.App (Core.Field (V 0) (FA "number")) (Core.DoubleLit (DhallDouble n))) =
                        return (Aeson.toJSON n)
                    outer (Core.App (Core.Field (V 0) (FA "string")) (Core.TextLit (Core.Chunks [] text))) =
                        return (toJSON text)
                    outer _ = Left (Unsupported e)

                outer value
        Core.Lam _ (Core.functionBindingAnnotation -> Core.Const Core.Type)
            (Core.Lam _ (Core.functionBindingAnnotation ->
                (Core.Record
                    [ ("array" , Core.recordFieldValue -> Core.Pi _ _ (Core.App Core.List (V 0)) (V 1))
                    , ("bool"  , Core.recordFieldValue -> Core.Pi _ _ Core.Bool (V 1))
                    , ("double", Core.recordFieldValue -> Core.Pi _ _ Core.Double (V 1))
                    , ("integer", Core.recordFieldValue -> Core.Pi _ _ Core.Integer (V 1))
                    , ("null"  , Core.recordFieldValue -> V 0)
                    , ("object", Core.recordFieldValue ->
                        Core.Pi _ _ (Core.App Core.List (Core.Record
                        [ ("mapKey", Core.recordFieldValue -> Core.Text)
                        , ("mapValue", Core.recordFieldValue -> V 0)
                        ])) (V 1))
                    , ("string", Core.recordFieldValue -> Core.Pi _ _ Core.Text (V 1))
                    ]
                ))
                value
            ) -> do
                let outer (Core.Field (V 0) (FA "null")) =
                        return Aeson.Null
                    outer (Core.App (Core.Field (V 0) (FA "bool")) (Core.BoolLit b)) =
                        return (Aeson.Bool b)
                    outer (Core.App (Core.Field (V 0) (FA "array")) (Core.ListLit _ xs)) = do
                        ys <- traverse outer (Foldable.toList xs)

                        return (Aeson.Array (Vector.fromList ys))
                    outer (Core.App (Core.Field (V 0) (FA "object")) (Core.ListLit _ xs)) = do
                        let inner (Core.RecordLit
                                    [ ("mapKey", Core.recordFieldValue -> Core.TextLit (Core.Chunks [] mapKey))
                                    , ("mapValue", Core.recordFieldValue -> mapExpression)]) = do
                                mapValue <- outer mapExpression

                                return (mapKey, mapValue)
                            inner _ = Left (Unsupported e)

                        ys <- traverse inner (Foldable.toList xs)

                        return (Aeson.Object (HashMap.fromList ys))
                    outer (Core.App (Core.Field (V 0) (FA "double")) (Core.DoubleLit (DhallDouble n))) =
                        return (Aeson.toJSON n)
                    outer (Core.App (Core.Field (V 0) (FA "integer")) (Core.IntegerLit n)) =
                        return (Aeson.toJSON n)
                    outer (Core.App (Core.Field (V 0) (FA "string")) (Core.TextLit (Core.Chunks [] text))) =
                        return (toJSON text)
                    outer _ = Left (Unsupported e)

                outer value
        _ -> Left (Unsupported e)

getContents :: Expr s Void -> Maybe (Text, Maybe (Expr s Void))
getContents (Core.App
                (Core.Field
                    _
                    (FA alternativeName)
                )
                expression
            ) = Just (alternativeName, Just expression)
getContents (Core.Field _ (FA alternativeName)) = Just (alternativeName, Nothing)
getContents _ = Nothing

isInlineNesting :: Expr s Void -> Bool
isInlineNesting (Core.App
                    (Core.Field
                        (Core.Union
                            [ ("Inline", Just (Core.Record []))
                            , ("Nested", Just Core.Text)
                            ]
                        )
                        (FA "Inline")
                    )
                    (Core.RecordLit [])
                )  = True
isInlineNesting (Core.Field
                    (Core.Union
                        [ ("Inline", Nothing)
                        , ("Nested", Just Core.Text)
                        ]
                    )
                    (FA "Inline")
                ) = True
isInlineNesting _ = False

toOrderedList :: Ord k => Map k v -> [(k, v)]
toOrderedList =
        Data.List.sortBy (Data.Ord.comparing fst)
    .   Dhall.Map.toList

-- | Omit record fields that are @null@
omitNull :: Value -> Value
omitNull (Object object) = Object fields
  where
    fields =HashMap.filter (/= Null) (fmap omitNull object)
omitNull (Array array) =
    Array (fmap omitNull array)
omitNull (String string) =
    String string
omitNull (Number number) =
    Number number
omitNull (Bool bool) =
    Bool bool
omitNull Null =
    Null

{-| Omit record fields that are @null@, arrays and records whose transitive
    fields are all null
-}
omitEmpty :: Value -> Value
omitEmpty (Object object) =
    if null fields then Null else Object fields
  where
    fields = HashMap.filter (/= Null) (fmap omitEmpty object)
omitEmpty (Array array) =
    if null elems then Null else Array elems
  where
    elems = Vector.filter (/= Null) (fmap omitEmpty array)
omitEmpty (String string) =
    String string
omitEmpty (Number number) =
    Number number
omitEmpty (Bool bool) =
    Bool bool
omitEmpty Null =
    Null

-- | Parser for command-line options related to omitting fields
parseOmission :: Parser (Value -> Value)
parseOmission =
        Options.Applicative.flag'
            omitEmpty
            (   Options.Applicative.long "omit-empty"
            <>  Options.Applicative.help "Omit record fields that are null or empty records"
            )

-- | Parser for command-line options related to preserving null fields.
parseNullPreservation :: Parser (Value -> Value)
parseNullPreservation =
        Options.Applicative.flag
            omitNull
            id
            (   Options.Applicative.long "preserve-null"
            <>  Options.Applicative.help "Preserve record fields that are null"
            )

-- | Combines parsers for command-line options related to preserving & omitting null fields.
parsePreservationAndOmission :: Parser (Value -> Value)
parsePreservationAndOmission = parseOmission <|> parseNullPreservation

{-| Specify whether or not to convert association lists of type
    @List { mapKey: Text, mapValue : v }@ to records
-}
data Conversion
    = NoConversion
    | Conversion { mapKey :: Text, mapValue :: Text }

defaultConversion :: Conversion
defaultConversion = Conversion
    { mapKey = "mapKey"
    , mapValue = "mapValue"
    }

{-| Convert association lists to homogeneous maps

    This converts an association list of the form:

    > [ { mapKey = k0, mapValue = v0 }, { mapKey = k1, mapValue = v1 } ]

    ... to a record of the form:

    > { k0 = v0, k1 = v1 }
-}
convertToHomogeneousMaps :: Conversion -> Expr s Void -> Expr s Void
convertToHomogeneousMaps NoConversion e0 = e0
convertToHomogeneousMaps (Conversion {..}) e0 = loop (Core.normalize e0)
  where
    loop e = case e of
        Core.Const a ->
            Core.Const a

        Core.Var v ->
            Core.Var v

        {- Minor hack: Don't descend into lambda, since the only thing it can
           possibly encode is a Boehm-Berarducci-encoded JSON value.  In such a
           case we do *not* want to perform this rewrite since it will
           interfere with decoding the value.
        -}
        Core.Lam cs a b ->
            Core.Lam cs a b

        Core.Pi cs a b c ->
            Core.Pi cs a b' c'
          where
            b' = loop b
            c' = loop c

        Core.App a b ->
            Core.App a' b'
          where
            a' = loop a
            b' = loop b

        Core.Let (Binding comment0 a src comment1 b comment2 c) d ->
            Core.Let (Binding comment0 a src comment1 b' comment2 c') d'
          where
            b' = fmap (fmap loop) b
            c' =            loop  c
            d' =            loop  d

        Core.Annot a b ->
            Core.Annot a' b'
          where
            a' = loop a
            b' = loop b

        Core.Bool ->
            Core.Bool

        Core.BoolLit a ->
            Core.BoolLit a

        Core.BoolAnd a b ->
            Core.BoolAnd a' b'
          where
            a' = loop a
            b' = loop b

        Core.BoolOr a b ->
            Core.BoolOr a' b'
          where
            a' = loop a
            b' = loop b

        Core.BoolEQ a b ->
            Core.BoolEQ a' b'
          where
            a' = loop a
            b' = loop b

        Core.BoolNE a b ->
            Core.BoolNE a' b'
          where
            a' = loop a
            b' = loop b

        Core.BoolIf a b c ->
            Core.BoolIf a' b' c'
          where
            a' = loop a
            b' = loop b
            c' = loop c

        Core.Natural ->
            Core.Natural

        Core.NaturalLit a ->
            Core.NaturalLit a

        Core.NaturalFold ->
            Core.NaturalFold

        Core.NaturalBuild ->
            Core.NaturalBuild

        Core.NaturalIsZero ->
            Core.NaturalIsZero

        Core.NaturalEven ->
            Core.NaturalEven

        Core.NaturalOdd ->
            Core.NaturalOdd

        Core.NaturalToInteger ->
            Core.NaturalToInteger

        Core.NaturalShow ->
            Core.NaturalShow

        Core.NaturalSubtract ->
            Core.NaturalSubtract

        Core.NaturalPlus a b ->
            Core.NaturalPlus a' b'
          where
            a' = loop a
            b' = loop b

        Core.NaturalTimes a b ->
            Core.NaturalTimes a' b'
          where
            a' = loop a
            b' = loop b

        Core.Integer ->
            Core.Integer

        Core.IntegerLit a ->
            Core.IntegerLit a

        Core.IntegerClamp ->
            Core.IntegerClamp

        Core.IntegerNegate ->
            Core.IntegerNegate

        Core.IntegerShow ->
            Core.IntegerShow

        Core.IntegerToDouble ->
            Core.IntegerToDouble

        Core.Double ->
            Core.Double

        Core.DoubleLit a ->
            Core.DoubleLit a

        Core.DoubleShow ->
            Core.DoubleShow

        Core.Text ->
            Core.Text

        Core.TextLit (Core.Chunks a b) ->
            Core.TextLit (Core.Chunks a' b)
          where
            a' = fmap (fmap loop) a

        Core.TextAppend a b ->
            Core.TextAppend a' b'
          where
            a' = loop a
            b' = loop b

        Core.TextReplace ->
            Core.TextReplace

        Core.TextShow ->
            Core.TextShow

        Core.List ->
            Core.List

        Core.ListLit a b ->
            case transform of
                Just c  -> loop c
                Nothing -> Core.ListLit a' b'
          where
            elements = Foldable.toList b

            toKeyValue :: Expr s Void -> Maybe (Text, Expr s Void)
            toKeyValue (Core.RecordLit m) = do
                guard (Foldable.length m == 2)

                key   <- Core.recordFieldValue <$> Dhall.Map.lookup mapKey   m
                value <- Core.recordFieldValue <$> Dhall.Map.lookup mapValue m

                keyText <- case key of
                    Core.TextLit (Core.Chunks [] keyText) ->
                        return keyText

                    Core.Field (Core.Union _) (FA keyText) ->
                        return keyText

                    _ ->
                        empty

                return (keyText, value)
            toKeyValue _ =
                empty

            transform =
                case elements of
                    [] ->
                        case a of
                            Just (Core.App Core.List (Core.Record m)) -> do
                                guard (Foldable.length m == 2)
                                guard (Dhall.Map.member mapKey   m)
                                guard (Dhall.Map.member mapValue m)
                                return (Core.RecordLit mempty)
                            _ -> empty

                    _  -> do
                        keyValues <- traverse toKeyValue elements

                        let recordLiteral = Core.makeRecordField <$>
                                Dhall.Map.fromList keyValues

                        return (Core.RecordLit recordLiteral)

            a' = fmap loop a
            b' = fmap loop b

        Core.ListAppend a b ->
            Core.ListAppend a' b'
          where
            a' = loop a
            b' = loop b

        Core.ListBuild ->
            Core.ListBuild

        Core.ListFold ->
            Core.ListFold

        Core.ListLength ->
            Core.ListLength

        Core.ListHead ->
            Core.ListHead

        Core.ListLast ->
            Core.ListLast

        Core.ListIndexed ->
            Core.ListIndexed

        Core.ListReverse ->
            Core.ListReverse

        Core.Optional ->
            Core.Optional

        Core.Some a ->
            Core.Some a'
          where
            a' = loop a

        Core.None ->
            Core.None

        Core.Record a ->
            Core.Record a'
          where
            a' = Lens.over Core.recordFieldExprs loop <$> a

        Core.RecordLit a ->
            Core.RecordLit a'
          where
            a' = Lens.over Core.recordFieldExprs loop <$> a

        Core.Union a ->
            Core.Union a'
          where
            a' = fmap (fmap loop) a

        Core.Combine cs a b c ->
            Core.Combine cs a b' c'
          where
            b' = loop b
            c' = loop c

        Core.CombineTypes cs a b ->
            Core.CombineTypes cs a' b'
          where
            a' = loop a
            b' = loop b

        Core.Prefer cs a b c ->
            Core.Prefer cs a b' c'
          where
            b' = loop b
            c' = loop c

        Core.RecordCompletion a b ->
            Core.RecordCompletion a' b'
          where
            a' = loop a
            b' = loop b

        Core.Merge a b c ->
            Core.Merge a' b' c'
          where
            a' =      loop a
            b' =      loop b
            c' = fmap loop c

        Core.ToMap a b ->
            Core.ToMap a' b'
          where
            a' =      loop a
            b' = fmap loop b

        Core.Field a b ->
            Core.Field a' b
          where
            a' = loop a

        Core.Project a b ->
            Core.Project a' b
          where
            a' = loop a

        Core.Assert a ->
            Core.Assert a'
          where
            a' = loop a

        Core.Equivalent cs a b ->
            Core.Equivalent cs a' b'
          where
            a' = loop a
            b' = loop b

        Core.With a b c ->
            Core.With a' b c'
          where
            a' = loop a
            c' = loop c

        Core.ImportAlt a b ->
            Core.ImportAlt a' b'
          where
            a' = loop a
            b' = loop b

        Core.Note a b ->
            Core.Note a b'
          where
            b' = loop b

        Core.Embed a ->
            Core.Embed a

-- | Parser for command-line options related to homogeneous map support
parseConversion :: Parser Conversion
parseConversion =
        conversion
    <|> noConversion
  where
    conversion = Conversion <$> parseKeyField <*> parseValueField
      where
        parseKeyField =
            Options.Applicative.strOption
                (   Options.Applicative.long "key"
                <>  Options.Applicative.help "Reserved key field name for association lists"
                <>  Options.Applicative.value "mapKey"
                <>  Options.Applicative.showDefaultWith Data.Text.unpack
                )

        parseValueField =
            Options.Applicative.strOption
                (   Options.Applicative.long "value"
                <>  Options.Applicative.help "Reserved value field name for association lists"
                <>  Options.Applicative.value "mapValue"
                <>  Options.Applicative.showDefaultWith Data.Text.unpack
                )

    noConversion =
        Options.Applicative.flag'
            NoConversion
            (   Options.Applicative.long "no-maps"
            <>  Options.Applicative.help "Disable conversion of association lists to homogeneous maps"
            )

-- | This option specifies how to encode @NaN@\/@Infinity@\/@-Infinity@
data SpecialDoubleMode
    = UseYAMLEncoding
    -- ^ YAML natively supports @NaN@\/@Infinity@\/@-Infinity@
    | ForbidWithinJSON
    -- ^ Forbid @NaN@\/@Infinity@\/@-Infinity@ because JSON doesn't support them
    | ApproximateWithinJSON
    -- ^ Encode @NaN@\/@Infinity@\/@-Infinity@ as
    --   @null@\/@1.7976931348623157e308@\/@-1.7976931348623157e308@,
    --   respectively

{-| Pre-process an expression containing @NaN@\/@Infinity@\/@-Infinity@,
    handling them as specified according to the `SpecialDoubleMode`
-}
handleSpecialDoubles
    :: SpecialDoubleMode -> Expr s Void -> Either CompileError (Expr s Void)
handleSpecialDoubles specialDoubleMode =
    Dhall.Optics.rewriteMOf Core.subExpressions rewrite
  where
    rewrite =
        case specialDoubleMode of
            UseYAMLEncoding       -> useYAMLEncoding
            ForbidWithinJSON      -> forbidWithinJSON
            ApproximateWithinJSON -> approximateWithinJSON

    useYAMLEncoding (Core.DoubleLit (DhallDouble n))
        | isInfinite n && 0 < n =
            return (Just (Core.TextLit (Core.Chunks [] "inf")))
        | isInfinite n && n < 0 =
            return (Just (Core.TextLit (Core.Chunks [] "-inf")))
        | isNaN n =
            return (Just (Core.TextLit (Core.Chunks [] "nan")))
    useYAMLEncoding _ =
        return Nothing

    forbidWithinJSON (Core.DoubleLit (DhallDouble n))
        | isInfinite n || isNaN n =
            Left (SpecialDouble n)
    forbidWithinJSON _ =
        return Nothing

    approximateWithinJSON (Core.DoubleLit (DhallDouble n))
        | isInfinite n && n > 0 =
            return (Just (Core.DoubleLit (DhallDouble 1.7976931348623157e308)))
        | isInfinite n && n < 0 =
            return (Just (Core.DoubleLit (DhallDouble (-1.7976931348623157e308))))
        -- Do nothing for @NaN@, which already encodes to @null@
    approximateWithinJSON _ =
        return Nothing

{-| Convert a piece of Text carrying a Dhall inscription to an equivalent JSON Value

>>> :set -XOverloadedStrings
>>> import Core
>>> Dhall.JSON.codeToValue defaultConversion ForbidWithinJSON Nothing "{ a = 1 }"
>>> Object (fromList [("a",Number 1.0)])
-}
codeToValue
  :: Conversion
  -> SpecialDoubleMode
  -> Maybe FilePath  -- ^ The source file path. If no path is given, imports
                     -- are resolved relative to the current directory.
  -> Text  -- ^ Input text.
  -> IO Value
codeToValue conversion specialDoubleMode mFilePath code = do
    parsedExpression <- Core.throws (Dhall.Parser.exprFromText (fromMaybe "(input)" mFilePath) code)

    let rootDirectory = case mFilePath of
            Nothing -> "."
            Just fp -> System.FilePath.takeDirectory fp

    resolvedExpression <- Dhall.Import.loadRelativeTo rootDirectory UseSemanticCache parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let convertedExpression =
            convertToHomogeneousMaps conversion resolvedExpression

    specialDoubleExpression <- Core.throws (handleSpecialDoubles specialDoubleMode convertedExpression)

    case dhallToJSON specialDoubleExpression of
      Left  err  -> Control.Exception.throwIO err
      Right json -> return json
