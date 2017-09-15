module Pos.Ssc.GodTossing.Constants
       ( GodTossingConstants (..)
       , godTossingConstants

       , mpcSendInterval
       , mdNoCommitmentsEpochThreshold
       ) where

import           Universum

import           Data.Aeson             (FromJSON (..), genericParseJSON)
import           Data.Tagged            (Tagged (..))
import           Data.Time.Units        (Microsecond)
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util          (sec)

import           Pos.Util.Config        (IsConfig (..), configParser, parseFromCslConfig)
import           Pos.Util.Util          ()

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

godTossingConstants :: GodTossingConstants
godTossingConstants = case parseFromCslConfig configParser of
    Left err -> error (toText ("Couldn't parse godtossing config: " ++ err))
    Right x  -> x

data GodTossingConstants = GodTossingConstants
    { -- | Length of interval for sending MPC message
      ccMpcSendInterval               :: !Word
      -- | VSS certificates max timeout to live (number of epochs)
    , ccVssMaxTTL                     :: !Word64
      -- | VSS certificates min timeout to live (number of epochs)
    , ccVssMinTTL                     :: !Word64
      -- | Threshold of epochs for malicious activity detection
    , ccMdNoCommitmentsEpochThreshold :: !Int
    }
    deriving (Show, Generic)

instance FromJSON GodTossingConstants where
    parseJSON = genericParseJSON defaultOptions

instance IsConfig GodTossingConstants where
    configPrefix = Tagged Nothing

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | Length of interval during which node should send her MPC message.
mpcSendInterval :: Microsecond
mpcSendInterval = sec . fromIntegral . ccMpcSendInterval $ godTossingConstants

-- | Number of epochs used by malicious actions detection to check if
-- our commitments are not included in blockchain.
mdNoCommitmentsEpochThreshold :: Integral i => i
mdNoCommitmentsEpochThreshold =
    fromIntegral . ccMdNoCommitmentsEpochThreshold $ godTossingConstants
