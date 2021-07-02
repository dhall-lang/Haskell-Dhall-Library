{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Dhall.JSON.Util
    ( pattern V
    , pattern FA
    ) where

import Data.Text  (Text)
import Dhall.Core (Expr, FieldSelection)

import qualified Dhall.Core as Core

pattern V :: Int -> Expr s a
pattern V n = Core.Var (Core.V "_" n)

pattern FA :: Text -> FieldSelection s
pattern FA t <- Core.FieldSelection _ _ t _
  where FA = Core.makeFieldSelection
