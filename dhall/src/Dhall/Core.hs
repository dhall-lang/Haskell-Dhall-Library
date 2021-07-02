{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

{-| This module contains the core calculus for the Dhall language.

    Dhall is essentially a fork of the @morte@ compiler but with more built-in
    functionality, better error messages, and Haskell integration
-}

module Dhall.Core (
    -- * Syntax
      Const(..)
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Scheme(..)
    , DhallDouble(..)
    , Var(..)
    , Binding(..)
    , makeBinding
    , Chunks(..)
    , PreferAnnotation(..)
    , RecordField(..)
    , makeRecordField
    , FunctionBinding(..)
    , makeFunctionBinding
    , FieldSelection (..)
    , makeFieldSelection
    , Expr(..)
    , MultiComment(..)
    , Comment(..)
    , CommentType(..)

    -- * Normalization
    , alphaNormalize
    , normalize
    , normalizeWith
    , normalizeWithM
    , Normalizer
    , NormalizerM
    , ReifiedNormalizer (..)
    , judgmentallyEqual
    , subst
    , shift
    , isNormalized
    , isNormalizedWith
    , denote
    , renote
    , shallowDenote
    , freeIn

    -- * Pretty-printing
    , pretty

    -- * Optics
    , subExpressions
    , chunkExprs
    , bindingExprs
    , recordFieldExprs
    , functionBindingExprs

    -- * Let-blocks
    , multiLet
    , wrapInLets
    , MultiLet(..)

    -- * Miscellaneous
    , internalError
    , reservedIdentifiers
    , escapeText
    , pathCharacter
    , throws
    , Eval.textShow
    , censorExpression
    , censorText
    , Syntax.desugarWith
    ) where

import Control.Exception         (Exception)
import Control.Monad.IO.Class    (MonadIO (..))
import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Dhall.Normalize
import Dhall.Pretty.Internal
import Dhall.Src                 (Src (..))
import Dhall.Syntax
import Instances.TH.Lift         ()
import Lens.Family               (over)

import qualified Control.Exception
import qualified Data.Text
import qualified Dhall.Eval        as Eval
import qualified Dhall.Syntax      as Syntax

-- | Pretty-print a value
pretty :: Pretty a => a -> Text
pretty = pretty_
{-# INLINE pretty #-}

{-| Escape a `Data.Text.Text` literal using Dhall's escaping rules

    Note that the result does not include surrounding quotes
-}
escapeText :: Text -> Text
escapeText = escapeText_
{-# INLINE escapeText #-}


{-| Utility used to implement the @--censor@ flag, by:

    * Replacing all `Src` text with spaces
    * Replacing all `Dhall.Syntax.Text` literals inside type errors with spaces
-}
censorExpression :: Expr Src a -> Expr Src a
censorExpression (TextLit chunks) = TextLit (censorChunks chunks)
censorExpression (Note src     e) = Note (censorSrc src) (censorExpression e)
censorExpression  e               = over subExpressions censorExpression e

censorChunks :: Chunks Src a -> Chunks Src a
censorChunks (Chunks xys z) = Chunks xys' z'
  where
    z' = censorText z

    xys' = [ (censorText x, censorExpression y) | (x, y) <- xys ]

{-| Utility used to censor `Data.Text.Text` by replacing all characters with a
    space
-}
censorText :: Text -> Text
censorText = Data.Text.map (\_ -> ' ')

censorSrc :: Src -> Src
censorSrc (Src { srcText = oldText, .. }) = Src { srcText = newText, .. }
  where
    newText = censorText oldText

{-| Convenience utility for converting `Either`-based exceptions to `IO`-based
    exceptions
-}
throws :: (Exception e, MonadIO io) => Either e a -> io a
throws (Left  e) = liftIO (Control.Exception.throwIO e)
throws (Right r) = return r
{-# INLINABLE throws #-}

{- $setup
>>> import qualified Codec.Serialise
>>> import qualified Dhall.Binary
>>> import Data.SpecialValues
>>> import Test.QuickCheck (Arbitrary(..), oneof, elements)
>>> :{
  instance Arbitrary DhallDouble where
    arbitrary = fmap DhallDouble (oneof [ arbitrary, elements specialValues ])
:}
-}
