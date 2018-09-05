{-# LANGUAGE RecordWildCards #-}

module Pos.Core.Genesis.Data
       ( GenesisData (..)
       ) where

import           Universum

import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..), fromJSField, mkObject)

import           Pos.Core.Common (SharedSeed)
import           Pos.Core.Slotting (Timestamp)
import           Pos.Core.Ssc (VssCertificatesMap)
import           Pos.Core.Update (BlockVersionData)

import           Pos.Core.Genesis.AvvmBalances
import           Pos.Core.Genesis.Delegation
import           Pos.Core.Genesis.NonAvvmBalances
import           Pos.Core.Genesis.ProtocolConstants
import           Pos.Core.Genesis.WStakeholders
import           Pos.Util.Json.Canonical ()

-- | Genesis data contains all data which determines consensus
-- rules. It must be same for all nodes. It's used to initialize
-- global state, slotting, etc.
data GenesisData = GenesisData
    { gdBootStakeholders :: !GenesisWStakeholders
    , gdHeavyDelegation  :: !GenesisDelegation
    , gdStartTime        :: !Timestamp
    , gdVssCerts         :: !VssCertificatesMap
    , gdNonAvvmBalances  :: !GenesisNonAvvmBalances
    , gdBlockVersionData :: !BlockVersionData
    , gdProtocolConsts   :: !GenesisProtocolConstants
    , gdAvvmDistr        :: !GenesisAvvmBalances
    , gdFtsSeed          :: !SharedSeed
    } deriving (Show, Eq)

instance Monad m => ToJSON m GenesisData where
    toJSON GenesisData {..} =
        mkObject
            [ ("bootStakeholders", toJSON gdBootStakeholders)
            , ("heavyDelegation", toJSON gdHeavyDelegation)
            , ("startTime", toJSON gdStartTime)
            , ("vssCerts", toJSON gdVssCerts)
            , ("nonAvvmBalances", toJSON gdNonAvvmBalances)
            , ("blockVersionData", toJSON gdBlockVersionData)
            , ("protocolConsts", toJSON gdProtocolConsts)
            , ("avvmDistr", toJSON gdAvvmDistr)
            , ("ftsSeed", toJSON gdFtsSeed)
            ]

instance (ReportSchemaErrors m) => FromJSON m GenesisData where
    fromJSON obj = do
        gdBootStakeholders <- fromJSField obj "bootStakeholders"
        gdHeavyDelegation <- fromJSField obj "heavyDelegation"
        gdStartTime <- fromJSField obj "startTime"
        -- note that we don't need to validate this map explicitly because
        -- the FromJSON instance of 'VssCertificatesMap' already does this
        gdVssCerts <- fromJSField obj "vssCerts"
        gdNonAvvmBalances <- fromJSField obj "nonAvvmBalances"
        gdBlockVersionData <- fromJSField obj "blockVersionData"
        gdProtocolConsts <- fromJSField obj "protocolConsts"
        gdAvvmDistr <- fromJSField obj "avvmDistr"
        gdFtsSeed <- fromJSField obj "ftsSeed"
        return GenesisData {..}
