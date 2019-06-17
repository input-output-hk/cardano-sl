{-# LANGUAGE OverloadedStrings #-}
module Pos.Util.Json.Parse
    ( tryParseString
    , wrapConstructor
    )
where

import           Universum

import           Data.Typeable                  ( typeRep )
import           Text.JSON.Canonical            ( JSValue(..)
                                                , JSString
                                                , fromJSString
                                                , ReportSchemaErrors(expected)
                                                , expectedButGotValue
                                                )

tryParseString
    :: forall a m e
     . (Typeable a, ReportSchemaErrors m, ToString e)
    => (Text -> Either e a)
    -> JSValue
    -> m a
tryParseString parser = \case
    JSString str -> case parser (toAesonStr str) of
        Right res -> pure res
        Left  err -> expected
            typeName
            (Just $ toAesonStr str <> ", err was: " <> toString err)
    val -> expectedButGotValue typeName val
    where typeName = show $ typeRep (Proxy @a)

wrapConstructor
    :: forall e a m
     . (Typeable a, ReportSchemaErrors m, ToString e)
    => Either e a
    -> m a
wrapConstructor = \case
    Left  err -> expected typeName (Just $ "error occurred: " <> toString err)
    Right x   -> pure x
    where typeName = show $ typeRep (Proxy @a)

toAesonStr :: IsString s => JSString -> s
toAesonStr = fromString . fromJSString
