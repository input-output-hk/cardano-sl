{-# OPTIONS_GHC -Wextra            #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Launcher.Environment
  ( substituteEnvVarsValue
  , substituteEnvVarsText
  )
where

import           Universum

import qualified Data.Aeson                       as AE
import qualified Data.Text                        as T
import           System.Environment                  (lookupEnv)
import qualified Text.Parser.Char                 as P
import qualified Text.Parser.Combinators          as P
import qualified Text.Parser.Token                as P
import qualified Text.Trifecta                    as P

import           Launcher.Logging                    (reportErrorDefault)

-- * Environment variable parsing and substitution for the launcher configuration file,
--   typically launcher-config.yaml.

-- | Represent an element of a parse of a string containing environment variable
--   references: either a plain text piece, or a variable reference.
data Chunk
  = EnvVar Text
  | Plain  Text
  deriving (Show)

-- | A parse is a sequence of variable references & plain text pieces.
parseEnvrefs :: Text -> P.Result [Chunk]
parseEnvrefs text = P.parseString pEnvrefs mempty (toString text)
  where
    pEnvrefs :: (Monad p, P.TokenParsing p) => p [Chunk]
    pEnvrefs = P.some $ P.choice [pPassiveText, pEnvvarRef]

    pEnvvarRef, pPassiveText :: (Monad p, P.TokenParsing p) => p Chunk
    pEnvvarRef   = P.char '$' >>
                   EnvVar <$> P.between (P.char '{') (P.char '}') pEnvvarName
    pPassiveText = P.some (P.noneOf "$") <&> Plain  . toText
    pEnvvarName :: (Monad p, P.TokenParsing p) => p Text
    pEnvvarName = (P.some $ P.choice [P.alphaNum, P.char '_']) <&> toText

-- | Substitute environment variable references. The 'desc' argument supplies
-- human-oriented context description in case of error.
substituteEnvVarsText :: Text -> Text -> IO Text
substituteEnvVarsText desc text = do
  chunks <- case parseEnvrefs text of
    P.Success r  -> pure r
    P.Failure ei -> do
      let err = desc <> "\n" <> (show $ P._errDoc ei)
      reportErrorDefault "config-parse-error.log" err
      error err
  substd <- forM chunks $
    \case Plain  x -> pure x
          EnvVar x -> do
            val <- lookupEnv $ toString x
            case val of
              Just x' -> pure $ toText x'
              Nothing -> do
                let err = desc <> "\n" <> "Reference to an undefined environment variable '"<> x <>"'"
                reportErrorDefault "config-parse-error.log" err
                error err
  pure $ T.concat substd

-- | Given an Aeson 'Value', parse and substitute environment variables in all
--   'AE.String' objects.  The 'desc' argument supplies context in case of error.
substituteEnvVarsValue :: Text -> AE.Value -> IO AE.Value
substituteEnvVarsValue desc (AE.String text) = AE.String <$> substituteEnvVarsText desc text
substituteEnvVarsValue desc (AE.Array xs)    = AE.Array  <$> traverse (substituteEnvVarsValue desc) xs
substituteEnvVarsValue desc (AE.Object o)    = AE.Object <$> traverse (substituteEnvVarsValue desc) o
substituteEnvVarsValue _    x                = pure x
