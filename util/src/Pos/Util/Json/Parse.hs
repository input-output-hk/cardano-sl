module Pos.Util.Json.Parse
       ( tryParseString
       , wrapConstructor
       ) where

import           Universum

import           Data.Typeable (typeRep)
import           Text.JSON.Canonical (JSValue (..),
                     ReportSchemaErrors (expected), expectedButGotValue)

tryParseString ::
       forall a m e. (Typeable a, ReportSchemaErrors m, ToString e)
    => (Text -> Either e a)
    -> JSValue
    -> m a
tryParseString parser =
    \case
        JSString str ->
            case parser (toText str) of
                Right res -> pure res
                Left (toString -> err) ->
                    expected typeName (Just $ str <> ", err was: " <> err)
        val -> expectedButGotValue typeName val
  where
    typeName = show $ typeRep (Proxy @a)

wrapConstructor ::
       forall e a m. (Typeable a, ReportSchemaErrors m, ToString e)
    => Either e a
    -> m a
wrapConstructor =
    \case
        Left err ->
            expected typeName (Just $ "error occurred: " <> toString err)
        Right x -> pure x
  where
    typeName = show $ typeRep (Proxy @a)
