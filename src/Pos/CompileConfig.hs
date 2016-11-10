{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time configuration support.
module Pos.CompileConfig
    ( CompileConfig (..)
    , compileConfig
    ) where

import qualified Data.Aeson.TH          as A
import           Data.FileEmbed         (embedFile, makeRelativeToProject)
import           Serokell.Aeson.Options (defaultOptions)
import           Universum

import           Pos.Util               (parseConf)

data CompileConfig = CompileConfig
    { ccK                       :: !Int
    , ccSlotDurationSec         :: !Int
    , ccNetworkDiameter         :: !Int
    , ccNeighboursSendThreshold :: !Int
    , ccGenesisN                :: !Int
    , ccMaxLocalTxs             :: !Word
    , ccMpcRelayInterval        :: !Int
    } deriving (Show)

$(A.deriveJSON defaultOptions ''CompileConfig)

compileConfig :: CompileConfig
compileConfig = parseConf $((makeRelativeToProject "constants.yaml" >>= embedFile))
