{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time configuration support.

module Pos.CompileConfig
    ( CompileConfig (..)
    , compileConfig
    ) where

import           Pos.CompileConfig.Parser
import           Pos.CompileConfig.Type
