{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time configuration support.
module Pos.CompileConfig
    ( CompileConfig (..)
    , compileConfig
    ) where

import qualified Data.Aeson.TH          as A
import           Data.FileEmbed         (embedFile, makeRelativeToProject)
import           Data.Maybe             (fromMaybe)
import           Data.Yaml              (decode)
import           Serokell.Aeson.Options (defaultOptions)
import           Universum

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

compileConfigStr :: ByteString
compileConfigStr = $(makeRelativeToProject "constants.yaml" >>= embedFile)

-- | Reading configuration from `rscoin.yaml` file and return it in data type.
compileConfig :: CompileConfig
compileConfig =
    fromMaybe (panic $ "FATAL: failed to parse constants.yaml") $
    decode compileConfigStr
