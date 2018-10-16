module Bench.Configuration
    ( benchProtocolConstants
    , benchProtocolMagic
    ) where

import           Pos.Core (ProtocolConstants (..), ProtocolMagic (..), ProtocolMagicId (..),
                           RequiresNetworkMagic (..), VssMaxTTL (..), VssMinTTL (..))

benchProtocolConstants :: ProtocolConstants
benchProtocolConstants = ProtocolConstants
    { pcK = 2
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

benchProtocolMagic :: ProtocolMagic
benchProtocolMagic = ProtocolMagic (ProtocolMagicId 55550001) NMMustBeNothing
