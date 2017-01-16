{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time configuration parser.

module Pos.CompileConfig.Parser where

import           Data.FileEmbed             (embedFile, makeRelativeToProject)
import           Data.Yaml                  (decodeEither)
import           Language.Haskell.TH.Syntax (lift)
import           Universum                  hiding (lift)

import           Pos.Aeson.CompileConfig    ()
import           Pos.CompileConfig.Type     (CompileConfig)


-- | Used in code compile-time configuration from /constants-*.yaml/ file.
compileConfig :: CompileConfig
compileConfig =
    $(do let file = $(embedFile =<< makeRelativeToProject
#ifdef DEV_MODE
                         "constants-dev.yaml"
#elif defined(WITH_WALLET)
                         "constants-wallet-prod.yaml"
#else
                         "constants-prod.yaml"
#endif
                     )
         case decodeEither file of
             Left a  -> fail (toString a)
             Right x -> lift (x :: CompileConfig))
