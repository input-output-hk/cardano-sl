module Pos.Core.Genesis.ProtocolConstants
       ( GenesisProtocolConstants (..)
       , genesisProtocolConstantsToProtocolConstants
       , genesisProtocolConstantsFromProtocolConstants
       ) where

import           Universum

import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     VssMaxTTL (..), VssMinTTL (..))
import           Pos.Crypto.Configuration (ProtocolMagic)

-- | 'GensisProtocolConstants' are not really part of genesis global state,
-- but they affect consensus, so they are part of 'GenesisSpec' and
-- 'GenesisData'.
data GenesisProtocolConstants = GenesisProtocolConstants
    { -- | Security parameter from the paper.
      gpcK             :: !Int
      -- | Magic constant for separating real/testnet.
    , gpcProtocolMagic :: !ProtocolMagic
      -- | VSS certificates max timeout to live (number of epochs).
    , gpcVssMaxTTL     :: !VssMaxTTL
      -- | VSS certificates min timeout to live (number of epochs).
    , gpcVssMinTTL     :: !VssMinTTL
    } deriving (Show, Eq, Generic)

genesisProtocolConstantsToProtocolConstants
    :: GenesisProtocolConstants
    -> ProtocolConstants
genesisProtocolConstantsToProtocolConstants GenesisProtocolConstants {..} =
    ProtocolConstants
        { pcK = gpcK
        , pcVssMinTTL = gpcVssMinTTL
        , pcVssMaxTTL = gpcVssMaxTTL
        }

genesisProtocolConstantsFromProtocolConstants
    :: ProtocolConstants
    -> ProtocolMagic
    -> GenesisProtocolConstants
genesisProtocolConstantsFromProtocolConstants ProtocolConstants {..} pm =
    GenesisProtocolConstants
        { gpcK = pcK
        , gpcProtocolMagic = pm
        , gpcVssMinTTL = pcVssMinTTL
        , gpcVssMaxTTL = pcVssMaxTTL
        }
