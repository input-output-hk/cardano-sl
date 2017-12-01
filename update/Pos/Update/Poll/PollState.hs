-- Types related Poll update system

module Pos.Update.Poll.PollState
       (
         -- * Types
         PollState (PollState)

       -- * Lenses
       -- * PollState
       , psBlockVersions
       , psAdoptedBV
       , psEpochProposers
       , psConfirmedANs
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

import           Control.Lens (makeLenses)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import           Pos.Core.Common (StakeholderId)
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Core.Update (ApplicationName, BlockVersion, BlockVersionData,
                                  NumSoftwareVersion, SoftwareVersion (..), UpId,
                                  UpdateProposal (..))
import           Pos.Lrc.DB.Issuers (IssuersStakes)
import           Pos.Lrc.Types (FullRichmenData)
import           Pos.Slotting.Types (SlottingData)
import           Pos.Update.Poll.Modifier (PollModifier (..))
import           Pos.Update.Poll.Types (BlockVersionState, ConfirmedProposalState, ProposalState,
                                        psProposal)
import           Pos.Util.Modifier (foldlMapModWKey', modifyHashMap)

data PollState = PollState
    { -- | All competing block versions with their states
      _psBlockVersions      :: !(HashMap BlockVersion BlockVersionState)
      -- | Presently adopted block version with its data
    , _psAdoptedBV          :: !(BlockVersion, BlockVersionData)
      -- | All stakeholders who made proposals in the current epoch
    , _psEpochProposers     :: !(HashSet StakeholderId)
      -- | All applications in use and their latest (confirmed) versions
    , _psConfirmedANs       :: !(HM.HashMap ApplicationName NumSoftwareVersion)
      -- | All confirmed software versions and their state
    , _psConfirmedProposals :: !(HM.HashMap SoftwareVersion ConfirmedProposalState)
      -- | All update proposals and their states
    , _psActiveProposals    :: !(HM.HashMap UpId ProposalState)
      -- | Update proposals for each application
    , _psActivePropsIdx     :: !(HM.HashMap ApplicationName (HashSet UpId))
      -- | Slotting data for this node
    , _psSlottingData       :: !(SlottingData)
      -- | Mapping between epochs and their richmen stake distribution
    , _psFullRichmenData    :: !(HM.HashMap EpochIndex FullRichmenData)
      -- | Mapping between epochs and stake of each of the epoch's slot's block issuer
    , _psIssuersStakes      :: !(HM.HashMap EpochIndex IssuersStakes)
    } deriving (Show, Eq, Generic)

makeLenses ''PollState

modifyPollState :: PollModifier -> PollState -> PollState
modifyPollState PollModifier {..} PollState {..} =
    PollState (modifyHashMap pmBVs _psBlockVersions)
              (fromMaybe _psAdoptedBV pmAdoptedBVFull)
              (fromMaybe _psEpochProposers pmEpochProposers)
              (modifyHashMap pmConfirmed _psConfirmedANs)
              (modifyHashMap pmConfirmedProps _psConfirmedProposals)
              (modifyHashMap pmActiveProps _psActiveProposals)
              (HM.filter (not . null) $ resultActiveProposals _psActivePropsIdx)
              (fromMaybe _psSlottingData pmSlottingData)
              _psFullRichmenData
              _psIssuersStakes
  where
    alterFun ui hs =
        let resHS = HS.delete ui hs
        in if null resHS then Nothing
                         else Just resHS

    addUpIdsToAppNameHS hashMap upId Nothing =
        HM.mapMaybe (alterFun upId) hashMap
    addUpIdsToAppNameHS hashMap upId (Just propSt) =
        let appName = svAppName . upSoftwareVersion . psProposal $ propSt
        in HM.insertWith HS.union appName (HS.singleton upId) hashMap

    resultActiveProposals hm =
        foldlMapModWKey' addUpIdsToAppNameHS
                         hm
                         pmActiveProps
