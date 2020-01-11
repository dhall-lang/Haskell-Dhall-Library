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
    , removeLetInLet
    , replaceOptionalBuildFold
    ) where

import Control.Applicative ((<|>))

import Dhall.Syntax
    ( Binding(..)
    , Const(..)
    , Directory(..)
    , Expr(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportType(..)
    , Var(..)
    , subExpressions
    )

import qualified Data.List.NonEmpty as NonEmpty
import qualified Dhall.Core         as Core
import qualified Dhall.Optics
import qualified Lens.Family

{-| Automatically improve a Dhall expression

    Currently this:

    * removes unused @let@ bindings with 'removeUnusedBindings'.
    * fixes @let a = x ≡ y@ to be @let a = assert : x ≡ y@
    * consolidates nested @let@ bindings to use a multiple-@let@ binding with 'removeLetInLet'
    * fixes paths of the form @.\/..\/foo@ to @..\/foo@
    * Replaces deprecated @Optional\/fold@ and @Optional\/build@ built-ins
-}
lint :: Expr s Import -> Expr s Import
lint = Dhall.Optics.rewriteOf subExpressions rewrite
  where
    rewrite e =
            fixAssert                e
        <|> removeUnusedBindings     e
        <|> fixParentPath            e
        <|> removeLetInLet           e
        <|> replaceOptionalBuildFold e

-- | Remove unused `Let` bindings.
removeUnusedBindings :: Eq a => Expr s a -> Maybe (Expr s a)
-- Don't remove assertions!
removeUnusedBindings (Let (Binding _ _ _ _ _ e) _)
    | isOrContainsAssert e = Nothing
removeUnusedBindings (Let (Binding _ a _ _ _ _) d)
    | not (V a 0 `Core.freeIn` d) =
        Just (Core.shift (-1) (V a 0) d)
removeUnusedBindings _ = Nothing

-- | Fix `Let` bindings  that the user probably meant to be `assert`s
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

-- | This replaces @Optional/fold@ and @Optional/build@, both of which can be
-- implemented within the language
replaceOptionalBuildFold :: Expr s a -> Maybe (Expr s a)
replaceOptionalBuildFold OptionalBuild =
    Just
        (Lam "a" (Const Type)
            (Lam "build"
                (Pi "optional" (Const Type)
                    (Pi "some" (Pi "_" "a" "optional")
                        (Pi "none" "optional" "optional")
                    )
                )
                (App (App (App "build" (App Optional "a")) (Lam "x" "a" (Some "x"))) (App None "a"))
            )
        )
replaceOptionalBuildFold OptionalFold =
    Just
        (Lam "a" (Const Type)
            (Lam "o" (App Optional "a")
                (Lam "optional" (Const Type)
                    (Lam "some" (Pi "_" "a" "optional")
                        (Lam "none" "optional"
                            (Merge
                                (RecordLit
                                    [ ("Some", "some")
                                    , ("None", "none")
                                    ]
                                )
                                "o"
                                Nothing
                            )
                        )
                    )
                )
            )
        )
replaceOptionalBuildFold _ =
    Nothing
