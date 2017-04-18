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
       , psFullRichmenData
       , psIssuersStakes
       ) where

import           Control.Lens                 (makeLenses)
import           Data.HashSet                 (HashSet)
import           Data.HashMap.Strict          (HashMap)
import           Universum

import           Pos.Core.Types               (BlockVersionData)
import           Pos.Lrc.DB.Issuers           (IssuersStakes)
import           Pos.Lrc.Types                (FullRichmenData)
import           Pos.Slotting.Types           (SlottingData)
import           Pos.Types                    (ApplicationName, BlockVersion,
                                               EpochIndex, NumSoftwareVersion,
                                               SoftwareVersion, StakeholderId)
import           Pos.Update.Core              (UpId)
import           Pos.Update.Poll              (BlockVersionState, ConfirmedProposalState,
                                               ProposalState)

data PollState = PollState
    { -- | All competing block versions with their states
      _psBlockVersions      :: !(HashMap BlockVersion BlockVersionState)
      -- | Presently adopted block version with its data
    , _psAdoptedBV          :: !(BlockVersion, BlockVersionData)
      -- | Software version of this node
    , _psSoftwareVersion    :: !SoftwareVersion
      -- | All stakeholders who made proposals in the current epoch
    , _psEpochProposers     :: !(HashSet StakeholderId)
      -- | All applications in use and their latest (confirmed) versions
    , _psConfirmedBVs       :: !(HashMap ApplicationName NumSoftwareVersion)
      -- | All proposed software versions and their state
    , _psConfirmedProposals :: !(HashMap SoftwareVersion ConfirmedProposalState)
      -- | All update proposals and their states
    , _psActiveProposals    :: !(HashMap UpId ProposalState)
      -- | Update proposals for each application
    , _psDelActivePropsIdx  :: !(HashMap ApplicationName (HashSet UpId))
      -- | Slotting data for this node
    , _psSlottingData       :: !SlottingData
      -- | Mapping between epochs and their richmen stake distribution
    , _psFullRichmenData    :: !(HashMap EpochIndex FullRichmenData)
      -- | Mapping between epochs and stake of each of the epoch's slot's block issuer
    , _psIssuersStakes      :: !(HashMap EpochIndex IssuersStakes)
    } deriving (Show, Eq)

makeLenses ''PollState
