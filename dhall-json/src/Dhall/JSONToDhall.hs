{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Convert JSON data to Dhall given a Dhall /type/ expression necessary to make the translation unambiguous.

    Reasonable requirements for conversion are:

    1. The Dhall type expression @/t/@ passed as an argument to @json-to-dhall@ should be a valid type of the resulting Dhall expression
    2. A JSON data produced by the corresponding @dhall-to-json@ from the Dhall expression of type @/t/@ should (under reasonable assumptions) reproduce the original Dhall expression using @json-to-dhall@ with type argument @/t/@

    Only a subset of Dhall types consisting of all the primitive types as well as @Optional@, @Union@ and @Record@ constructs, is used for reading JSON data:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@s
    * @List@s
    * @Optional@ values
    * unions
    * records

    Additionally, you can read in arbitrary JSON data into a Dhall value of
    type @https://prelude.dhall-lang.org/JSON/Type@ if you don't know the
    schema of the JSON data in advance.

    This library can be used to implement an executable which takes any data
    serialisation format which can be parsed as an Aeson @Value@ and converts
    the result to a Dhall value. One such executable is @json-to-dhall@ which
    is used in the examples below.

== Primitive types

    JSON @Bool@s translate to Dhall bools:

> $ json-to-dhall Bool <<< 'true'
> True
> $ json-to-dhall Bool <<< 'false'
> False

    JSON numbers translate to Dhall numbers:

> $ json-to-dhall Integer <<< 2
> +2
> $ json-to-dhall Natural <<< 2
> 2
> $ json-to-dhall Double <<< -2.345
> -2.345

    Dhall @Text@ corresponds to JSON text:

> $ json-to-dhall Text <<< '"foo bar"'
> "foo bar"


== Lists and records

    Dhall @List@s correspond to JSON lists:

> $ json-to-dhall 'List Integer' <<< '[1, 2, 3]'
> [ +1, +2, +3 ]


    Dhall __records__ correspond to JSON records:

> $ json-to-dhall '{foo : List Integer}' <<< '{"foo": [1, 2, 3]}'
> { foo = [ +1, +2, +3 ] }


    Note, that by default, only the fields required by the Dhall type argument are parsed (as you commonly will not need all the data), the remaining ones being ignored:

> $ json-to-dhall '{foo : List Integer}' <<< '{"foo": [1, 2, 3], "bar" : "asdf"}'
> { foo = [ +1, +2, +3 ] }


    If you do need to make sure that Dhall fully reflects JSON record data comprehensively, @--records-strict@ flag should be used:

> $ json-to-dhall --records-strict '{foo : List Integer}' <<< '{"foo": [1, 2, 3], "bar" : "asdf"}'
> Error: Key(s) @bar@ present in the JSON object but not in the corresponding Dhall record. This is not allowed in presence of --records-strict:


    By default, JSON key-value arrays will be converted to Dhall records:

> $ json-to-dhall '{ a : Integer, b : Text }' <<< '[{"key":"a", "value":1}, {"key":"b", "value":"asdf"}]'
> { a = +1, b = "asdf" }


    Attempting to do the same with @--no-keyval-arrays@ on will result in error:

> $ json-to-dhall --no-keyval-arrays '{ a : Integer, b : Text }' <<< '[{"key":"a", "value":1}, {"key":"b", "value":"asdf"}]'
> Error: JSON (key-value) arrays cannot be converted to Dhall records under --no-keyval-arrays flag:

    Conversion of the homogeneous JSON maps to the corresponding Dhall association lists by default:

> $ json-to-dhall 'List { mapKey : Text, mapValue : Text }' <<< '{"foo": "bar"}'
> [ { mapKey = "foo", mapValue = "bar" } ]

    The map keys can even be union types instead of `Text`:

> $ json-to-dhall 'List { mapKey : < A | B >, mapValue : Natural }' <<< '{"A": 1, "B": 2}'
> [ { mapKey = < A | B >.A, mapValue = 1 }, { mapKey = < A | B >.B, mapValue = 2 } ]

    Flag @--no-keyval-maps@ switches off this mechanism (if one would ever need it):

> $ json-to-dhall --no-keyval-maps 'List { mapKey : Text, mapValue : Text }' <<< '{"foo": "bar"}'
> Error: Homogeneous JSON map objects cannot be converted to Dhall association lists under --no-keyval-arrays flag


== Optional values and unions

    Dhall @Optional@ Dhall type allows null or missing JSON values:

> $ json-to-dhall "Optional Integer" <<< '1'
> Some +1

> $ json-to-dhall "Optional Integer" <<< null
> None Integer

> $ json-to-dhall '{ a : Integer, b : Optional Text }' <<< '{ "a": 1 }'
{ a = +1, b = None Text }



    For Dhall __union types__ the correct value will be based on matching the type of JSON expression:

> $ json-to-dhall 'List < Left : Text | Right : Integer >' <<< '[1, "bar"]'
> [ < Left : Text | Right : Integer >.Right +1
  , < Left : Text | Right : Integer >.Left "bar"
  ]

> $ json-to-dhall '{foo : < Left : Text | Right : Integer >}' <<< '{ "foo": "bar" }'
> { foo = < Left : Text | Right : Integer >.Left "bar" }

    In presence of multiple potential matches, the first will be selected by default:

> $ json-to-dhall '{foo : < Left : Text | Middle : Text | Right : Integer >}' <<< '{ "foo": "bar"}'
> { foo = < Left : Text | Middle : Text | Right : Integer >.Left "bar" }

    This will result in error if @--unions-strict@ flag is used, with the list of alternative matches being reported (as a Dhall list)

> $ json-to-dhall --unions-strict '{foo : < Left : Text | Middle : Text | Right : Integer >}' <<< '{ "foo": "bar"}'
> Error: More than one union component type matches JSON value
> ...
> Possible matches:
< Left : Text | Middle : Text | Right : Integer >.Left "bar"
> --------
< Left : Text | Middle : Text | Right : Integer >.Middle "bar"

== Weakly-typed JSON

If you don't know the JSON's schema in advance, you can decode into the most
general schema possible:

> $ cat ./schema.dhall
> https://prelude.dhall-lang.org/JSON/Type

> $ json-to-dhall ./schema.dhall <<< '[ { "foo": null, "bar": [ 1.0, true ] } ]'
>   λ(JSON : Type)
> → λ(string : Text → JSON)
> → λ(number : Double → JSON)
> → λ(object : List { mapKey : Text, mapValue : JSON } → JSON)
> → λ(array : List JSON → JSON)
> → λ(bool : Bool → JSON)
> → λ(null : JSON)
> → array
>   [ object
>     [ { mapKey = "foo", mapValue = null }
>     , { mapKey = "bar", mapValue = array [ number 1.0, bool True ] }
>     ]
>   ]

You can also mix and match JSON fields whose schemas are known or unknown:

> $ cat ./mixed.dhall
> List
> { foo : Optional Natural
> , bar : https://prelude.dhall-lang.org/JSON/Type
> }

> $ json-to-dhall ./mixed.dhall <<< '[ { "foo": null, "bar": [ 1.0, true ] } ]'
> [ { bar =
>         λ(JSON : Type)
>       → λ(string : Text → JSON)
>       → λ(number : Double → JSON)
>       → λ(object : List { mapKey : Text, mapValue : JSON } → JSON)
>       → λ(array : List JSON → JSON)
>       → λ(bool : Bool → JSON)
>       → λ(null : JSON)
>       → array [ number 1.0, bool True ]
>   , foo =
>       None Natural
>   }
> ]

-}

module Dhall.JSONToDhall (
    -- * JSON to Dhall
      parseConversion
    , Conversion(..)
    , defaultConversion
    , resolveSchemaExpr
    , typeCheckSchemaExpr
    , dhallFromJSON

    -- * Exceptions
    , CompileError(..)
    , showCompileError
    ) where

import           Control.Applicative ((<|>))
import           Control.Exception (Exception, throwIO)
import           Control.Monad.Catch (throwM, MonadCatch)
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Either (rights)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import           Data.List ((\\))
import           Data.Monoid ((<>))
import           Data.Scientific (floatingOrInteger, toRealFloat)
import qualified Data.Sequence as Seq
import qualified Data.String
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Vector as Vector
import           Data.Void (Void)
import qualified Options.Applicative as O
import           Options.Applicative (Parser)

import           Dhall.JSON.Util (pattern V)
import qualified Dhall.Core as D
import           Dhall.Core (Expr(App), Chunks(..), DhallDouble(..))
import qualified Dhall.Import
import qualified Dhall.Map as Map
import qualified Dhall.Parser
import           Dhall.Parser (Src)
import qualified Dhall.TypeCheck as D

-- ---------------
-- Command options
-- ---------------

-- | Standard parser for options related to the conversion method
parseConversion :: Parser Conversion
parseConversion = Conversion <$> parseStrict
                             <*> parseKVArr
                             <*> parseKVMap
                             <*> parseUnion
                             <*> parseOmissibleLists
  where
    parseStrict =
            O.flag' True
            (  O.long "records-strict"
            <> O.help "Fail if any YAML fields are missing from the expected Dhall type"
            )
        <|> O.flag' False
            (  O.long "records-loose"
            <> O.help "Tolerate YAML fields not present within the expected Dhall type"
            )
        <|> pure True

    parseKVArr  =  O.switch
                (  O.long "no-keyval-arrays"
                <> O.help "Disable conversion of key-value arrays to records"
                )
    parseKVMap  =  O.switch
                (  O.long "no-keyval-maps"
                <> O.help "Disable conversion of homogeneous map objects to association lists"
                )
    parseOmissibleLists = O.switch
                          ( O.long "omissible-lists"
                          <> O.help "Tolerate missing list values, they are assumed empty"
                          )

-- | Parser for command options related to treating union types
parseUnion :: Parser UnionConv
parseUnion =
        uFirst
    <|> uNone
    <|> uStrict
    <|> pure UFirst -- defaulting to UFirst
  where
    uFirst  =  O.flag' UFirst
            (  O.long "unions-first"
            <> O.help "The first value with the matching type (succefully parsed all the way down the tree) is accepted, even if not the only posible match. (DEFAULT)"
            )
    uNone   =  O.flag' UNone
            (  O.long "unions-none"
            <> O.help "Unions not allowed"
            )
    uStrict =  O.flag' UStrict
            (  O.long "unions-strict"
            <> O.help "Error if more than one union values match the type (and parse successfully)"
            )

-- ----------
-- Conversion
-- ----------

-- | JSON-to-dhall translation options
data Conversion = Conversion
    { strictRecs     :: Bool
    , noKeyValArr    :: Bool
    , noKeyValMap    :: Bool
    , unions         :: UnionConv
    , omissibleLists :: Bool
    } deriving Show

data UnionConv = UFirst | UNone | UStrict deriving (Show, Read, Eq)

-- | Default conversion options
defaultConversion :: Conversion
defaultConversion = Conversion
    { strictRecs     = False
    , noKeyValArr    = False
    , noKeyValMap    = False
    , unions         = UFirst
    , omissibleLists = False
    }

-- | The 'Expr' type concretization used throughout this module
type ExprX = Expr Src Void

-- | Parse schema code and resolve imports
resolveSchemaExpr :: Text  -- ^ type code (schema)
                  -> IO ExprX
resolveSchemaExpr code = do
    parsedExpression <-
      case Dhall.Parser.exprFromText "\n\ESC[1;31mSCHEMA\ESC[0m" code of
        Left  err              -> throwIO err
        Right parsedExpression -> return parsedExpression
    Dhall.Import.load parsedExpression

{-| Check that the Dhall type expression actually has type 'Type'
>>> :set -XOverloadedStrings
>>> import Dhall.Core

>>> typeCheckSchemaExpr id =<< resolveSchemaExpr "List Natural"
App List Natural

>>> typeCheckSchemaExpr id =<< resolveSchemaExpr "+1"
*** Exception:
Error: Schema expression is successfully parsed but has Dhall type:
Integer
Expected Dhall type: Type
Parsed expression: +1
-}
typeCheckSchemaExpr :: (Exception e, MonadCatch m)
                    => (CompileError -> e) -> ExprX -> m ExprX
typeCheckSchemaExpr compileException expr =
  case D.typeOf expr of -- check if the expression has type
    Left  err -> throwM . compileException $ TypeError err
    Right t   -> case t of -- check if the expression has type Type
      D.Const D.Type -> return expr
      _              -> throwM . compileException $ BadDhallType t expr

keyValMay :: A.Value -> Maybe (Text, A.Value)
keyValMay (A.Object o) = do
     A.String k <- HM.lookup "key" o
     v <- HM.lookup "value" o
     return (k, v)
keyValMay _ = Nothing

{-| The main conversion function. Traversing\/zipping Dhall /type/ and Aeson value trees together to produce a Dhall /term/ tree, given 'Conversion' options:

>>> :set -XOverloadedStrings
>>> import qualified Dhall.Core as D
>>> import qualified Dhall.Map as Map
>>> import qualified Data.Aeson as A
>>> import qualified Data.HashMap.Strict as HM

>>> s = D.Record (Map.fromList [("foo", D.Integer)])
>>> v = A.Object (HM.fromList [("foo", A.Number 1)])
>>> dhallFromJSON defaultConversion s v
Right (RecordLit (fromList [("foo",IntegerLit 1)]))

-}
dhallFromJSON
  :: Conversion -> ExprX -> A.Value -> Either CompileError ExprX
dhallFromJSON (Conversion {..}) expressionType =
    loop (D.alphaNormalize (D.normalize expressionType))
  where
    -- any ~> Union
    loop t@(D.Union tm) v = do
      let f key maybeType =
            case maybeType of
              Just _type -> do
                expression <- loop _type v

                return (D.App (D.Field t key) expression)

              Nothing -> do
                case v of
                    A.String text | key == text -> do
                        return (D.Field t key)
                    _ -> do
                        Left (Mismatch t v)

      case (unions, rights (toList (Map.mapWithKey f tm))) of
        (UNone  , _         ) -> Left (ContainsUnion t)
        (UStrict, xs@(_:_:_)) -> Left (UndecidableUnion t v xs)
        (_      , [ ]       ) -> Left (Mismatch t v)
        (UFirst , x:_       ) -> Right x
        (UStrict, [x]       ) -> Right x

    -- object ~> Record
    loop (D.Record r) v@(A.Object o)
        | extraKeys <- HM.keys o \\ Map.keys r
        , strictRecs && not (null extraKeys)
        = Left (UnhandledKeys extraKeys (D.Record r) v)
        | otherwise
        = let f :: Text -> ExprX -> Either CompileError ExprX
              f k t | Just value <- HM.lookup k o
                    = loop t value
                    | App D.Optional t' <- t
                    = Right (App D.None t')
                    | App D.List _ <- t
                    , omissibleLists
                    = Right (D.ListLit (Just t) [])
                    | otherwise
                    = Left (MissingKey k t v)
           in D.RecordLit <$> Map.traverseWithKey f r

    -- key-value list ~> Record
    loop t@(D.Record _) v@(A.Array a)
        | not noKeyValArr
        , os :: [A.Value] <- toList a
        , Just kvs <- traverse keyValMay os
        = loop t (A.Object $ HM.fromList kvs)
        | noKeyValArr
        = Left (NoKeyValArray t v)
        | otherwise
        = Left (Mismatch t v)

    -- object ~> List (key, value)
    loop t@(App D.List (D.Record r)) v@(A.Object o)
        | not noKeyValMap
        , ["mapKey", "mapValue"] == Map.keys r
        , Just mapKey   <- Map.lookup "mapKey" r
        , Just mapValue <- Map.lookup "mapValue" r
        = do
          keyExprMap <- traverse (loop mapValue) o

          toKey <- do
              case mapKey of
                  D.Text    -> return (\key -> D.TextLit (Chunks [] key))
                  D.Union _ -> return (\key -> D.Field mapKey key)
                  _         -> Left (Mismatch t v)

          let f :: (Text, ExprX) -> ExprX
              f (key, val) = D.RecordLit ( Map.fromList
                  [ ("mapKey"  , toKey key)
                  , ("mapValue", val)
                  ] )

          let records = (fmap f . Seq.fromList . HM.toList) keyExprMap

          let typeAnn = if HM.null o then Just t else Nothing

          return (D.ListLit typeAnn records)
        | noKeyValMap
        = Left (NoKeyValMap t v)
        | otherwise
        = Left (Mismatch t v)

    -- array ~> List
    loop (App D.List t) (A.Array a)
        = let f :: [ExprX] -> ExprX
              f es = D.ListLit
                       (if null es then Just (App D.List t) else Nothing)
                       (Seq.fromList es)
           in f <$> traverse (loop t) (toList a)

    -- null ~> List
    loop t@(App D.List _) (A.Null)
        = if omissibleLists
          then Right (D.ListLit (Just t) [])
          else Left (Mismatch t A.Null)

    -- number ~> Integer
    loop D.Integer (A.Number x)
        | Right n <- floatingOrInteger x :: Either Double Integer
        = Right (D.IntegerLit n)
        | otherwise
        = Left (Mismatch D.Integer (A.Number x))

    -- number ~> Natural
    loop D.Natural (A.Number x)
        | Right n <- floatingOrInteger x :: Either Double Integer
        , n >= 0
        = Right (D.NaturalLit (fromInteger n))
        | otherwise
        = Left (Mismatch D.Natural (A.Number x))

    -- number ~> Double
    loop D.Double (A.Number x)
        = Right (D.DoubleLit $ DhallDouble $ toRealFloat x)

    -- string ~> Text
    loop D.Text (A.String t)
        = Right (D.TextLit (Chunks [] t))

    -- bool ~> Bool
    loop D.Bool (A.Bool t)
        = Right (D.BoolLit t)

    -- null ~> Optional
    loop (App D.Optional expr) A.Null
        = Right $ App D.None expr

    -- value ~> Optional
    loop (App D.Optional expr) value
        = D.Some <$> loop expr value

    -- Arbitrary JSON ~> https://prelude.dhall-lang.org/JSON/Type (< v13.0.0)
    loop
      (D.Pi _ (D.Const D.Type)
          (D.Pi _
              (D.Record
                  [ ("array" , D.Pi _ (D.App D.List (V 0)) (V 1))
                  , ("bool"  , D.Pi _ D.Bool (V 1))
                  , ("null"  , V 0)
                  , ("number", D.Pi _ D.Double (V 1))
                  , ("object", D.Pi _ (D.App D.List (D.Record [ ("mapKey", D.Text), ("mapValue", V 0)])) (V 1))
                  , ("string", D.Pi _ D.Text (V 1))
                  ]
              )
              (V 1)
          )
      )
      value = do
          let outer (A.Object o) =
                  let inner (key, val) =
                          D.RecordLit
                              [ ("mapKey"  , D.TextLit (D.Chunks [] key))
                              , ("mapValue", outer val                  )
                              ]

                      elements = Seq.fromList (fmap inner (HM.toList o))

                      elementType
                          | null elements =
                              Just (D.App D.List (D.Record [ ("mapKey", D.Text), ("mapValue", "JSON") ]))
                          | otherwise =
                              Nothing

                      keyValues = D.ListLit elementType elements

                  in  (D.App (D.Field "json" "object") keyValues)
              outer (A.Array a) =
                  let elements = Seq.fromList (fmap outer (Vector.toList a))

                      elementType
                          | null elements = Just (D.App D.List "JSON")
                          | otherwise     = Nothing

                  in  D.App (D.Field "json" "array") (D.ListLit elementType elements)
              outer (A.String s) =
                  D.App (D.Field "json" "string") (D.TextLit (D.Chunks [] s))
              outer (A.Number n) =
                  D.App (D.Field "json" "number") (D.DoubleLit (DhallDouble (toRealFloat n)))
              outer (A.Bool b) =
                  D.App (D.Field "json" "bool") (D.BoolLit b)
              outer A.Null =
                  D.Field "json" "null"

          let result =
                D.Lam "JSON" (D.Const D.Type)
                    (D.Lam "json"
                        (D.Record
                            [ ("array" , D.Pi "_" (D.App D.List "JSON") "JSON")
                            , ("bool"  , D.Pi "_" D.Bool "JSON")
                            , ("null"  , "JSON")
                            , ("number", D.Pi "_" D.Double "JSON")
                            , ("object", D.Pi "_" (D.App D.List (D.Record [ ("mapKey", D.Text), ("mapValue", "JSON")])) "JSON")
                            , ("string", D.Pi "_" D.Text "JSON")
                            ]
                        )
                        (outer value)
                    )

          return result

    -- Arbitrary JSON ~> https://prelude.dhall-lang.org/JSON/Type (v13.0.0 <=)
    loop
      (D.Pi _ (D.Const D.Type)
          (D.Pi _
              (D.Record
                  [ ("array" , D.Pi _ (D.App D.List (V 0)) (V 1))
                  , ("bool"  , D.Pi _ D.Bool (V 1))
                  , ("double", D.Pi _ D.Double (V 1))
                  , ("integer", D.Pi _ D.Integer (V 1))
                  , ("null"  , V 0)
                  , ("object", D.Pi _ (D.App D.List (D.Record [ ("mapKey", D.Text), ("mapValue", V 0)])) (V 1))
                  , ("string", D.Pi _ D.Text (V 1))
                  ]
              )
              (V 1)
          )
      )
      value = do
          let outer (A.Object o) =
                  let inner (key, val) =
                          D.RecordLit
                              [ ("mapKey"  , D.TextLit (D.Chunks [] key))
                              , ("mapValue", outer val                  )
                              ]

                      elements = Seq.fromList (fmap inner (HM.toList o))

                      elementType
                          | null elements =
                              Just (D.App D.List (D.Record [ ("mapKey", D.Text), ("mapValue", "JSON") ]))
                          | otherwise =
                              Nothing

                      keyValues = D.ListLit elementType elements

                  in  (D.App (D.Field "json" "object") keyValues)
              outer (A.Array a) =
                  let elements = Seq.fromList (fmap outer (Vector.toList a))

                      elementType
                          | null elements = Just (D.App D.List "JSON")
                          | otherwise     = Nothing

                  in  D.App (D.Field "json" "array") (D.ListLit elementType elements)
              outer (A.String s) =
                  D.App (D.Field "json" "string") (D.TextLit (D.Chunks [] s))
              outer (A.Number n) =
                  D.App (D.Field "json" "double") (D.DoubleLit (DhallDouble (toRealFloat n)))
              outer (A.Bool b) =
                  D.App (D.Field "json" "bool") (D.BoolLit b)
              outer A.Null =
                  D.Field "json" "null"

          let result =
                D.Lam "JSON" (D.Const D.Type)
                    (D.Lam "json"
                        (D.Record
                            [ ("array" , D.Pi "_" (D.App D.List "JSON") "JSON")
                            , ("bool"  , D.Pi "_" D.Bool "JSON")
                            , ("double", D.Pi "_" D.Double "JSON")
                            , ("integer", D.Pi "_" D.Integer "JSON")
                            , ("null"  , "JSON")
                            , ("object", D.Pi "_" (D.App D.List (D.Record [ ("mapKey", D.Text), ("mapValue", "JSON")])) "JSON")
                            , ("string", D.Pi "_" D.Text "JSON")
                            ]
                        )
                        (outer value)
                    )

          return result

    -- fail
    loop expr value
        = Left (Mismatch expr value)


-- ----------
-- EXCEPTIONS
-- ----------

red, purple, green
    :: (Monoid a, Data.String.IsString a) => a -> a
red    s = "\ESC[1;31m" <> s <> "\ESC[0m" -- bold
purple s = "\ESC[1;35m" <> s <> "\ESC[0m" -- bold
green  s = "\ESC[0;32m" <> s <> "\ESC[0m" -- plain

showExpr :: ExprX   -> String
showExpr dhall = Text.unpack (D.pretty dhall)

showJSON :: A.Value -> String
showJSON value = BSL8.unpack (encodePretty value)

data CompileError
  -- Dhall shema
  = TypeError (D.TypeError Src Void)
  | BadDhallType
      ExprX -- Expression type
      ExprX -- Whole expression
  -- generic mismatch (fallback)
  | Mismatch
      ExprX   -- Dhall expression
      A.Value -- Aeson value
  -- record specific
  | MissingKey     Text  ExprX A.Value
  | UnhandledKeys [Text] ExprX A.Value
  | NoKeyValArray        ExprX A.Value
  | NoKeyValMap          ExprX A.Value
  -- union specific
  | ContainsUnion        ExprX
  | UndecidableUnion     ExprX A.Value [ExprX]

instance Show CompileError where
    show = showCompileError "JSON" showJSON

instance Exception CompileError

showCompileError :: String -> (A.Value -> String) -> CompileError -> String
showCompileError format showValue = let prefix = red "\nError: "
          in \case
    TypeError e -> show e

    BadDhallType t e -> prefix
      <> "Schema expression is successfully parsed but has Dhall type:\n"
      <> showExpr t <> "\nExpected Dhall type: Type"
      <> "\nParsed expression: "
      <> showExpr e <> "\n"

    ContainsUnion e -> prefix
      <> "Dhall type expression contains union type:\n"
      <> showExpr e <> "\nwhile it is forbidden by option "
      <> green "--unions-none\n"

    UndecidableUnion e v xs -> prefix
      <> "More than one union component type matches " <> format <> " value"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n\nPossible matches:\n\n" -- Showing all the allowed matches
      <> Text.unpack (Text.intercalate sep $ D.pretty <$> xs)
        where sep = red "\n--------\n" :: Text

    Mismatch e v -> prefix
      <> "Dhall type expression and " <> format <> " value do not match:"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n"

    MissingKey k e v -> prefix
      <> "Key " <> purple (Text.unpack k) <> ", expected by Dhall type:\n"
      <> showExpr e
      <> "\nis not present in " <> format <> " object:\n"
      <> showValue v <> "\n"

    UnhandledKeys ks e v -> prefix
      <> "Key(s) " <> purple (Text.unpack (Text.intercalate ", " ks))
      <> " present in the " <> format <> " object but not in the expected Dhall record type. This is not allowed unless you enable the "
      <> green "--records-loose" <> " flag:"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n"

    NoKeyValArray e v -> prefix
      <> "" <> format <> " (key-value) arrays cannot be converted to Dhall records under "
      <> green "--no-keyval-arrays" <> " flag"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n"

    NoKeyValMap e v -> prefix
      <> "Homogeneous " <> format <> " map objects cannot be converted to Dhall association lists under "
      <> green "--no-keyval-arrays" <> " flag"
      <> "\n\nExpected Dhall type:\n" <> showExpr e
      <> "\n\n" <> format <> ":\n"  <> showValue v
      <> "\n"
