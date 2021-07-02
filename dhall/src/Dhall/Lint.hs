{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module contains the implementation of the @dhall lint@ command

module Dhall.Lint
    ( -- * Lint
      lint
    , removeUnusedBindings
    , fixAssert
    , fixParentPath
    , addPreludeExtensions
    , removeLetInLet
    , useToMap
    ) where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty(..))

import Dhall.Syntax
    ( Binding (..)
    , Chunks (..)
    , Directory (..)
    , Expr (..)
    , File (..)
    , FilePrefix (..)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    , URL (..)
    , Var (..)
    , subExpressions
    )

import qualified Data.Foldable      as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map           as Map
import qualified Data.Text          as Text
import qualified Dhall.Core         as Core
import qualified Dhall.Map
import qualified Dhall.Optics
import qualified Lens.Family

{-| Automatically improve a Dhall expression

    Currently this:

    * removes unused @let@ bindings with 'removeUnusedBindings'.
    * fixes @let a = x ≡ y@ to be @let a = assert : x ≡ y@
    * consolidates nested @let@ bindings to use a multiple-@let@ binding with 'removeLetInLet'
    * fixes paths of the form @.\/..\/foo@ to @..\/foo@
-}
lint :: Eq s => Expr s Import -> Expr s Import
lint =  Dhall.Optics.rewriteOf subExpressions rewrite
  where
    rewrite e =
            fixAssert                e
        <|> removeUnusedBindings     e
        <|> fixParentPath            e
        <|> removeLetInLet           e
        <|> addPreludeExtensions     e
        <|> sortImports              e

-- | Remove unused `Let` bindings.
removeUnusedBindings :: Eq a => Expr s a -> Maybe (Expr s a)
-- Don't remove assertions!
removeUnusedBindings (Let (Binding _ _ _ _ _ _ e) _)
    | isOrContainsAssert e = Nothing
removeUnusedBindings (Let (Binding _ a _ _ _ _ _) d)
    | not (V a 0 `Core.freeIn` d) =
        Just (Core.shift (-1) (V a 0) d)
removeUnusedBindings _ = Nothing

-- | Fix `Let` bindings  that the user probably meant to be @assert@s
fixAssert :: Expr s a -> Maybe (Expr s a)
fixAssert (Let (Binding { value = v@(Core.shallowDenote -> Equivalent {}), ..}) body) =
    Just (Let (Binding { value = Assert v, .. }) body)
fixAssert (Let binding body@(Core.shallowDenote -> Equivalent {})) =
    Just (Let binding (Assert body))
fixAssert _ =
    Nothing

-- | This transforms @.\/..\/foo@ into @..\/foo@
fixParentPath :: Expr s Import -> Maybe (Expr s Import)
fixParentPath (Embed oldImport) = do
    let Import{..} = oldImport

    let ImportHashed{..} = importHashed

    case importType of
        Local Here File{ directory = Directory { components }, .. }
            | Just nonEmpty <- NonEmpty.nonEmpty components
            , NonEmpty.last nonEmpty == ".." -> do
                let newDirectory =
                        Directory { components = NonEmpty.init nonEmpty }

                let newImportType =
                        Local Parent File{ directory = newDirectory, .. }

                let newImportHashed =
                        ImportHashed { importType = newImportType, .. }

                let newImport = Import { importHashed = newImportHashed, .. }

                Just (Embed newImport)
        _ ->
            Nothing
fixParentPath _  = Nothing

{-| This transforms @https://prelude.dhall-lang.org/…/foo@ to
    @https://prelude.dhall-lang.org/…/foo.dhall@
-}
addPreludeExtensions :: Expr s Import -> Maybe (Expr s Import)
addPreludeExtensions (Embed oldImport) = do
    let Import{ importHashed = oldImportHashed, .. } = oldImport

    let ImportHashed{ importType = oldImportType, .. } = oldImportHashed

    case oldImportType of
        Remote URL{ path = oldPath, ..}
            | authority == "prelude.dhall-lang.org" ->
                case oldPath of
                    File{ file = oldFile, .. }
                        | not (Text.isSuffixOf ".dhall" oldFile) -> do
                            let newFile = oldFile <> ".dhall"

                            let newPath = File{ file = newFile, .. }

                            let newImportType = Remote URL{ path = newPath, .. }

                            let newImportHashed =
                                    ImportHashed{ importType = newImportType, .. }

                            let newImport =
                                    Import{ importHashed = newImportHashed, .. }

                            return (Embed newImport)
                    _ ->
                        Nothing
        _ -> do
            Nothing
addPreludeExtensions _ = Nothing

isOrContainsAssert :: Expr s a -> Bool
isOrContainsAssert (Assert _) = True
isOrContainsAssert e = Lens.Family.anyOf subExpressions isOrContainsAssert e

-- | The difference between
--
-- > let x = 1 let y = 2 in x + y
--
-- and
--
-- > let x = 1 in let y = 2 in x + y
--
-- is that in the second expression, the inner 'Let' is wrapped by a 'Note'.
--
-- We remove such a 'Note' in order to consolidate nested let-blocks into a
-- single one.
removeLetInLet :: Expr s a -> Maybe (Expr s a)
removeLetInLet (Let binding (Note _ l@Let{})) = Just (Let binding l)
removeLetInLet _ = Nothing

-- | This replaces a record of key-value pairs with the equivalent use of
--   @toMap@
--
-- This is currently not used by @dhall lint@ because this would sort @Map@
-- keys, which is not necessarily a behavior-preserving change, but is still
-- made available as a convenient rewrite rule.  For example,
-- @{json,yaml}-to-dhall@ use this rewrite to simplify their output.
useToMap :: Expr s a -> Maybe (Expr s a)
useToMap
    (ListLit
        t@(Just
            (Core.shallowDenote -> App
                (Core.shallowDenote -> List)
                (Core.shallowDenote -> Record
                    (Dhall.Map.sort ->
                        [ ("mapKey", Core.shallowDenote . Core.recordFieldValue -> Text)
                        , ("mapValue", _)
                        ]
                    )
                )
            )
        )
        []
    ) =
        Just (ToMap (RecordLit []) t)
useToMap (ListLit _ keyValues)
    | not (null keyValues)
    , Just keyValues' <- traverse convert keyValues =
        Just
            (ToMap
                (RecordLit (Dhall.Map.fromList (Foldable.toList keyValues')))
                Nothing
            )
  where
    convert keyValue =
        case Core.shallowDenote keyValue of
            RecordLit
                (Dhall.Map.sort ->
                    [ ("mapKey"  , Core.shallowDenote . Core.recordFieldValue -> TextLit (Chunks [] key))
                    , ("mapValue", value)
                    ]
                ) ->
                    Just (key, value)
            _ ->
                Nothing
useToMap _ =
    Nothing

-- | This sorts `let` bindings to move imports to the front if doing so does not
-- change the behavior of the code.
sortImports :: Eq s => Expr s Import -> Maybe (Expr s Import)
sortImports oldExpression@(Let binding0 oldBody0)
    | oldExpression == newExpression = Nothing
    | otherwise                      = Just newExpression
  where
    toBool (Embed _ ) = False
    toBool (Note _ e) = toBool e
    toBool  _         = True

    process (seen, index) Binding{..} oldBody function = (pair, pairs, newBody)
      where
        order =
            if b then index else Map.findWithDefault (0 :: Int) variable seen

        b = toBool value

        pair = (order, function)

        ~(pairs, newBody) =
            label (Map.insert variable order seen, index + 1) oldBody

    label state (Let binding oldBody) = (pair : pairs, newBody)
      where
        function = Let binding

        ~(pair, pairs, newBody) = process state binding oldBody function

    label state (Note src (Let binding oldBody)) = (pair : pairs, newBody)
      where
        function e = Note src (Let binding e)

        ~(pair, pairs, newBody) = process state binding oldBody function

    label _ body =
        ([], body)

    ~(pairs0, newBody0) = (pair :| pairs, newBody)
      where
        function = Let binding0

        ~(pair, pairs, newBody) =
            process (Map.empty, 1) binding0 oldBody0 function

    sortedFunctions = fmap snd (NonEmpty.sortWith fst pairs0)

    newExpression = foldr id newBody0 sortedFunctions
sortImports _ = Nothing
