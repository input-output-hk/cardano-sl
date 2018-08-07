module Pos.Core.Genesis.ProtocolConstants
       ( GenesisProtocolConstants (..)
       , genesisProtocolConstantsToProtocolConstants
       , genesisProtocolConstantsFromProtocolConstants
       ) where

import           Universum

import           Data.Aeson.Options (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Text.JSON.Canonical (FromJSON (..), Int54, JSValue (..),
                     ReportSchemaErrors, ToJSON (..), fromJSField, mkObject)

import           Pos.Core.Genesis.Canonical ()
import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     VssMaxTTL (..), VssMinTTL (..))
import           Pos.Crypto.Configuration (ProtocolMagic (..))

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

instance Monad m => ToJSON m GenesisProtocolConstants where
    toJSON GenesisProtocolConstants {..} =
        mkObject
            -- 'k' definitely won't exceed the limit
            [ ("k", pure . JSNum . fromIntegral $ gpcK)
            , ("protocolMagic", toJSON (getProtocolMagic gpcProtocolMagic))
            , ("vssMaxTTL", toJSON gpcVssMaxTTL)
            , ("vssMinTTL", toJSON gpcVssMinTTL)
            ]

instance ReportSchemaErrors m => FromJSON m GenesisProtocolConstants where
    fromJSON obj = do
        gpcK <- fromIntegral @Int54 <$> fromJSField obj "k"
        gpcProtocolMagic <- ProtocolMagic <$> fromJSField obj "protocolMagic"
        gpcVssMaxTTL <- fromJSField obj "vssMaxTTL"
        gpcVssMinTTL <- fromJSField obj "vssMinTTL"
        return GenesisProtocolConstants {..}

deriveJSON defaultOptions ''GenesisProtocolConstants

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
