module Pos.Core.Genesis.Data
       ( GenesisData (..)
       ) where

import           Universum

import           Pos.Core.Common (SharedSeed)
import           Pos.Core.Slotting (Timestamp)
import           Pos.Core.Update (BlockVersionData)

import           Pos.Core.Genesis.AvvmBalances
import           Pos.Core.Genesis.Delegation
import           Pos.Core.Genesis.NonAvvmBalances
import           Pos.Core.Genesis.ProtocolConstants
import           Pos.Core.Genesis.VssCertificatesMap
import           Pos.Core.Genesis.WStakeholders

-- | Genesis data contains all data which determines consensus
-- rules. It must be same for all nodes. It's used to initialize
-- global state, slotting, etc.
data GenesisData = GenesisData
    { gdBootStakeholders :: !GenesisWStakeholders
    , gdHeavyDelegation  :: !GenesisDelegation
    , gdStartTime        :: !Timestamp
    , gdVssCerts         :: !GenesisVssCertificatesMap
    , gdNonAvvmBalances  :: !GenesisNonAvvmBalances
    , gdBlockVersionData :: !BlockVersionData
    , gdProtocolConsts   :: !GenesisProtocolConstants
    , gdAvvmDistr        :: !GenesisAvvmBalances
    , gdFtsSeed          :: !SharedSeed
    } deriving (Show, Eq)
