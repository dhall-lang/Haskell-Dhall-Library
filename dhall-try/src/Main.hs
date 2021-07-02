{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson.Encode.Pretty
import qualified Data.IORef
import qualified Data.JSString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.JSON.Yaml
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified GHCJS.Foreign.Callback

import Control.Exception      (Exception, SomeException)
import Data.JSString          (JSString)
import Data.Text              (Text)
import GHCJS.Foreign.Callback (Callback)

foreign import javascript unsafe "input.getValue()" getInput :: IO JSString

foreign import javascript unsafe "input.on('change', $1)" registerInterpret :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "dhallTab.onclick = $1" registerDhallOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "jsonTab.onclick = $1" registerJSONOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "yamlTab.onclick = $1" registerYAMLOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "typeTab.onclick = $1" registerTypeOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "hashTab.onclick = $1" registerHashOutput :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "output.setValue($1)" setOutput_ :: JSString -> IO ()

foreign import javascript unsafe "output.setOption('mode', $1)" setMode_ :: JSString -> IO ()

foreign import javascript unsafe "selectTab($1, $2)" selectTab :: JSString -> JSString -> IO ()

fixup :: Text -> Text
fixup = Data.Text.replace "\ESC[1;31mError\ESC[0m" "Error"

setOutput :: Text -> IO ()
setOutput = setOutput_ . Data.JSString.pack . Data.Text.unpack

errOutput :: Exception e => e -> IO ()
errOutput = setOutput . fixup . Data.Text.pack . show

setMode :: Mode -> IO ()
setMode Dhall = setMode_ "haskell"
setMode Type  = setMode_ "haskell"
setMode JSON  = setMode_ "javascript"
setMode YAML  = setMode_ "yaml"
setMode Hash  = setMode_ "null"

jsonConfig :: Data.Aeson.Encode.Pretty.Config
jsonConfig =
    Data.Aeson.Encode.Pretty.Config
        { Data.Aeson.Encode.Pretty.confIndent =
            Data.Aeson.Encode.Pretty.Spaces 2
        , Data.Aeson.Encode.Pretty.confCompare =
            compare
        , Data.Aeson.Encode.Pretty.confNumFormat =
            Data.Aeson.Encode.Pretty.Generic
        , Data.Aeson.Encode.Pretty.confTrailingNewline =
            False
        }

data Mode = Dhall | Type | JSON | YAML | Hash deriving (Show)

main :: IO ()
main = do
    modeRef <- Data.IORef.newIORef YAML

    let prettyExpression =
              Pretty.renderStrict
            . Dhall.Pretty.layout
            . Dhall.Pretty.prettyExpr

    let interpret = do
            inputJSString <- getInput

            let inputString = Data.JSString.unpack inputJSString
            let inputText   = Data.Text.pack inputString

            case Dhall.Parser.exprFromText "(input)" inputText of
                Left exception ->
                    errOutput exception
                Right parsedExpression -> do
                  eitherResolvedExpression <- Control.Exception.try (Dhall.Import.load parsedExpression)
                  case eitherResolvedExpression of
                      Left exception ->
                          errOutput (exception :: SomeException)
                      Right resolvedExpression ->
                          case Dhall.TypeCheck.typeOf resolvedExpression of
                              Left exception ->
                                  errOutput exception
                              Right inferredType -> do
                                  mode <- Data.IORef.readIORef modeRef
                                  case mode of
                                      Dhall -> do
                                          let normalizedExpression =
                                                  Dhall.Core.normalize resolvedExpression
                                          let dhallText =
                                                  prettyExpression normalizedExpression
                                          setOutput dhallText

                                      Type -> do
                                          let typeText =
                                                  prettyExpression inferredType

                                          setOutput typeText

                                      JSON ->
                                          case Dhall.JSON.dhallToJSON resolvedExpression of
                                              Left exception ->
                                                  errOutput exception
                                              Right value -> do
                                                  let jsonBytes = Data.Aeson.Encode.Pretty.encodePretty' jsonConfig value
                                                  case Data.Text.Lazy.Encoding.decodeUtf8' jsonBytes of
                                                      Left exception ->
                                                          errOutput exception
                                                      Right jsonText ->
                                                          setOutput (Data.Text.Lazy.toStrict jsonText)
                                      YAML ->
                                          case Dhall.JSON.dhallToJSON resolvedExpression of
                                              Left exception ->
                                                  errOutput exception
                                              Right value -> do
                                                  let yamlBytes = Dhall.JSON.Yaml.jsonToYaml value False False
                                                  case Data.Text.Encoding.decodeUtf8' yamlBytes of
                                                      Left exception ->
                                                          errOutput exception
                                                      Right yamlText ->
                                                          setOutput yamlText

                                      Hash ->
                                          setOutput (Dhall.Import.hashExpressionToCode (Dhall.Core.alphaNormalize (Dhall.Core.normalize resolvedExpression)))

    interpret

    interpretAsync <- GHCJS.Foreign.Callback.asyncCallback interpret

    registerInterpret interpretAsync

    let registerTabCallback mode tabName registerCallback = do
            let callback = do
                    Data.IORef.writeIORef modeRef mode

                    selectTab "mode-tab" tabName

                    setMode mode

                    interpret

            callbackAsync <- GHCJS.Foreign.Callback.asyncCallback callback

            registerCallback callbackAsync

    registerTabCallback Dhall "dhall-tab" registerDhallOutput
    registerTabCallback Type  "type-tab"  registerTypeOutput
    registerTabCallback JSON  "json-tab"  registerJSONOutput
    registerTabCallback YAML  "yaml-tab"  registerYAMLOutput
    registerTabCallback Hash  "hash-tab"  registerHashOutput
