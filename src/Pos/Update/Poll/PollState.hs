{-# LANGUAGE TemplateHaskell #-}

-- Types related Poll update system

module Pos.Update.Poll.PollState
       (
         -- * Types
         PollState (..)

       -- * Lenses
       -- * PollState
       , psBlockVersions
       , psAdoptedBV
       , psSoftwareVersion
       , psEpochProposers
       , psConfirmedBVs
       , psConfirmedProposals
       , psActiveProposals
       , psDelActivePropsIdx
       , psSlottingData
       , psUndecidedProposals
       , psDecidedProposals
       , psStakePerEpoch
       , psMultiRichmenStake
       , psIssuersStakes
       ) where

import           Control.Lens                 (makeLenses)
import           Data.HashSet                 (HashSet)
import           Data.HashMap.Strict          (HashMap)
import           Universum

import           Pos.Core.Types               (BlockVersionData, Coin)
import           Pos.Lrc.DB.Issuers           (IssuersStakes)
import           Pos.Slotting.Types           (SlottingData)
import           Pos.Ssc.GodTossing.Toss.Pure (MultiRichmenStake)
import           Pos.Types                    (ApplicationName, BlockVersion,
                                               EpochIndex, NumSoftwareVersion,
                                               SoftwareVersion, StakeholderId)
import           Pos.Update.Core              (UpId)
import           Pos.Update.Poll              (BlockVersionState,
                                               ConfirmedProposalState,
                                               DecidedProposalState, ProposalState,
                                               UndecidedProposalState)
import           Pos.Util.Modifier            (MapModifier)

data PollState = PollState
    { -- | All proposed block versions with their states
      _psBlockVersions      :: !(MapModifier BlockVersion BlockVersionState)
      -- | Presently adopted block version with its data
    , _psAdoptedBV          :: !(BlockVersion, BlockVersionData)
      -- | Software version of this node
    , _psSoftwareVersion    :: !SoftwareVersion
      -- | All stakeholders who made proposals in the current epoch
    , _psEpochProposers     :: !(HashSet StakeholderId)
      -- | All applications in use and their latest (confirmed) versions
    , _psConfirmedBVs       :: !(MapModifier ApplicationName NumSoftwareVersion)
      -- | All proposed software versions and their state
    , _psConfirmedProposals :: !(MapModifier SoftwareVersion ConfirmedProposalState)
      -- | All update proposals and their states
    , _psActiveProposals    :: !(MapModifier UpId ProposalState)
      -- | Update proposals for each application
    , _psDelActivePropsIdx  :: !(HashMap ApplicationName (HashSet UpId))
      -- | Slotting data for this node
    , _psSlottingData       :: !SlottingData
      -- | State of undecided proposals
    , _psUndecidedProposals :: ![UndecidedProposalState]
      -- | State of decided proposals
    , _psDecidedProposals   :: ![DecidedProposalState]
      -- | Mapping between epochs and stake to them allotted
    , _psStakePerEpoch      :: !(HashMap EpochIndex Coin)
    , _psMultiRichmenStake  :: !MultiRichmenStake
      -- | Mapping between epochs and stake of each of the epoch's slot's block issuer
    , _psIssuersStakes      :: !(HashMap EpochIndex IssuersStakes)
    } deriving (Show, Eq)

makeLenses ''PollState
