{-# LANGUAGE Rank2Types #-}

-- | Utilities for passing runtime SSC configuration.

module Pos.Ssc.Configuration
    ( SscConfiguration (..)
    , HasSscConfiguration
    , sscConfiguration
    , withSscConfiguration
    , mpcSendInterval
    , mdNoCommitmentsEpochThreshold
    , noReportNoSecretsForEpoch1
    ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import           Data.Reflection (Given (..), give)
import           Serokell.Aeson.Options (defaultOptions)

import           Data.Time.Units (Microsecond, fromMicroseconds)

type HasSscConfiguration = Given SscConfiguration

withSscConfiguration :: SscConfiguration -> (HasSscConfiguration => r) -> r
withSscConfiguration = give

sscConfiguration :: HasSscConfiguration => SscConfiguration
sscConfiguration = given

data SscConfiguration = SscConfiguration
    { -- | Length of interval for sending MPC message
      ccMpcSendInterval               :: !Word
      -- | Threshold of epochs for malicious activity detection
    , ccMdNoCommitmentsEpochThreshold :: !Int
      -- | Don't print “SSC couldn't compute seed” for the first epoch.
    , ccNoReportNoSecretsForEpoch1    :: !Bool
    }
    deriving (Show, Generic)

instance FromJSON SscConfiguration where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON SscConfiguration where
    toJSON = genericToJSON defaultOptions

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | Length of interval during which node should send her MPC message.
mpcSendInterval :: HasSscConfiguration => Microsecond
mpcSendInterval = fromMicroseconds . (*) 1000000 . fromIntegral . ccMpcSendInterval $ sscConfiguration

-- | Number of epochs used by malicious actions detection to check if
-- our commitments are not included in blockchain.
mdNoCommitmentsEpochThreshold :: (HasSscConfiguration, Integral i) => i
mdNoCommitmentsEpochThreshold =
    fromIntegral . ccMdNoCommitmentsEpochThreshold $ sscConfiguration

-- | In the first mainnet version we messed up the calculation of the
-- initial richmen set, and the richmen weren't sending commitments. It has
-- been fixed, but now and for eternity the node prints “SSC couldn't
-- compute seed” after processing blocks from the first epoch. This flag
-- silences that message.
noReportNoSecretsForEpoch1 :: HasSscConfiguration => Bool
noReportNoSecretsForEpoch1 = ccNoReportNoSecretsForEpoch1 $ sscConfiguration
