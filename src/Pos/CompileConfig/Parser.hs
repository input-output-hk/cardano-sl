-- | Compile-time configuration parser.

module Pos.CompileConfig.Parser
   ( compileConfig
   ) where

import           Data.Yaml               (parseJSON)
import           Universum

import           Pos.Aeson.CompileConfig ()
import           Pos.CompileConfig.Type  (CompileConfig)
import           Pos.Util.Config         (parseFromCslConfig)


compileConfig :: CompileConfig
compileConfig = case parseFromCslConfig parseJSON of
    Left err -> error (toText ("Couldn't parse compileConfig: " ++ err))
    Right x  -> x
