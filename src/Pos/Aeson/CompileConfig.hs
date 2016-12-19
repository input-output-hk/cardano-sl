{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.CompileConfig
       (
       ) where

import           Data.Aeson.TH          (deriveFromJSON)
import           Pos.CompileConfig.Type (CompileConfig)
import           Serokell.Aeson.Options (defaultOptions)

deriveFromJSON defaultOptions ''CompileConfig
