{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Genesis.Data
       ( GenesisData (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..), fromJSField,
                     mkObject)

import           Pos.Chain.Ssc.VssCertificatesMap (VssCertificatesMap)
import           Pos.Chain.Update.BlockVersionData (BlockVersionData)
import           Pos.Core.Common (SharedSeed)
import           Pos.Core.Slotting (Timestamp)

import           Pos.Chain.Genesis.AvvmBalances
import           Pos.Chain.Genesis.Delegation
import           Pos.Chain.Genesis.NonAvvmBalances
import           Pos.Chain.Genesis.ProtocolConstants
import           Pos.Chain.Genesis.WStakeholders
import           Pos.Util.Json.Canonical (SchemaError)

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

instance MonadError SchemaError m => FromJSON m GenesisData where
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
