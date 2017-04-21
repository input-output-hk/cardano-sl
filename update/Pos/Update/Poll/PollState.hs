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
       , psEpochProposers
       , psConfirmedBVs
       , psConfirmedProposals
       , psActiveProposals
       , psActivePropsIdx
       , psSlottingData
       , psFullRichmenData
       , psIssuersStakes

       -- * Functions
       , modifyPollState
       ) where

import           Universum

import           Control.Lens                 (makeLenses)
import           Data.HashSet                 (HashSet)
import qualified Data.HashMap.Strict          as HM

import           Pos.Core.Types               (ApplicationName, BlockVersion,
                                               BlockVersionData, EpochIndex,
                                               NumSoftwareVersion, SoftwareVersion,
                                               StakeholderId)
import           Pos.Lrc.DB.Issuers           (IssuersStakes)
import           Pos.Lrc.Types                (FullRichmenData)
import           Pos.Slotting.Types           (SlottingData)
import           Pos.Update.Core              (UpId)
import           Pos.Update.Poll.Types        (BlockVersionState, ConfirmedProposalState,
                                               PollModifier (..), ProposalState)
import           Pos.Util.Modifier            (modifyHashMap)

data PollState = PollState
    { -- | All competing block versions with their states
      _psBlockVersions      :: !(HashMap BlockVersion BlockVersionState)
      -- | Presently adopted block version with its data
    , _psAdoptedBV          :: !(BlockVersion, BlockVersionData)
      -- | All stakeholders who made proposals in the current epoch
    , _psEpochProposers     :: !(HashSet StakeholderId)
      -- | All applications in use and their latest (confirmed) versions
    , _psConfirmedBVs       :: !(HM.HashMap ApplicationName NumSoftwareVersion)
      -- | All confirmed software versions and their state
    , _psConfirmedProposals :: !(HM.HashMap SoftwareVersion ConfirmedProposalState)
      -- | All update proposals and their states
    , _psActiveProposals    :: !(HM.HashMap UpId ProposalState)
      -- | Update proposals for each application
    , _psActivePropsIdx     :: !(HM.HashMap ApplicationName (HashSet UpId))
      -- | Slotting data for this node
    , _psSlottingData       :: !SlottingData
      -- | Mapping between epochs and their richmen stake distribution
    , _psFullRichmenData    :: !(HM.HashMap EpochIndex FullRichmenData)
      -- | Mapping between epochs and stake of each of the epoch's slot's block issuer
    , _psIssuersStakes      :: !(HM.HashMap EpochIndex IssuersStakes)
    } deriving (Show, Eq)

makeLenses ''PollState

modifyPollState :: PollModifier -> PollState -> PollState
modifyPollState PollModifier {..} PollState {..} =
    PollState (modifyHashMap pmBVs _psBlockVersions)
              (fromMaybe _psAdoptedBV pmAdoptedBVFull)
              (fromMaybe _psEpochProposers pmEpochProposers)
              (modifyHashMap pmConfirmed _psConfirmedBVs)
              (modifyHashMap pmConfirmedProps _psConfirmedProposals)
              (modifyHashMap pmActiveProps _psActiveProposals)
              _psActivePropsIdx
              (fromMaybe _psSlottingData pmSlottingData)
              _psFullRichmenData
              _psIssuersStakes
