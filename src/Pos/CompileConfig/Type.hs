{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time configuration type.

module Pos.CompileConfig.Type
    ( CompileConfig (..)
    ) where

import qualified Data.Aeson.TH              as A
import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Aeson.Options     (defaultOptions)
import           Universum

data CompileConfig = CompileConfig
    { ccK                       :: !Int
    , ccSlotDurationSec         :: !Int
    , ccNetworkDiameter         :: !Int
    , ccNeighboursSendThreshold :: !Int
    , ccGenesisN                :: !Int
    , ccMaxLocalTxs             :: !Word
    , ccMpcRelayInterval        :: !Int
    , ccDefaultPeers            :: ![[Char]]
    , ccSysTimeBroadcastSlots   :: !Int
    , ccMpcSendInterval         :: !Word
    } deriving (Show, Lift)

$(A.deriveFromJSON defaultOptions ''CompileConfig)
