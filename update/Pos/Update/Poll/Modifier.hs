-- | Functions working with PollModifier.

module Pos.Update.Poll.Modifier
       (
         -- * Poll modifier
         PollModifier (..)
       , pmBVsL
       , pmAdoptedBVFullL
       , pmConfirmedL
       , pmConfirmedPropsL
       , pmActivePropsL
       , pmSlottingDataL
       , pmEpochProposersL

       , modifyPollModifier
       ) where

import           Universum

import           Control.Lens (makeLensesFor)
import           Data.Default (Default (def))
import           Data.Semigroup (Semigroup)

import           Pos.Core.Common (StakeholderId)
import           Pos.Core.Update (ApplicationName, BlockVersion, BlockVersionData,
                                  NumSoftwareVersion, SoftwareVersion, UpId)
import           Pos.Slotting.Types (SlottingData)
import           Pos.Update.Poll.Types (BlockVersionState, ConfirmedProposalState, ProposalState)
import           Pos.Util.Modifier (MapModifier)

-- | PollModifier is used in verification. It represents operation which
-- one should apply to global state to obtain result of application of
-- MemPool or blocks which are verified.
data PollModifier = PollModifier
    { pmBVs            :: !(MapModifier BlockVersion BlockVersionState)
    , pmAdoptedBVFull  :: !(Maybe (BlockVersion, BlockVersionData))
    , pmConfirmed      :: !(MapModifier ApplicationName NumSoftwareVersion)
    , pmConfirmedProps :: !(MapModifier SoftwareVersion ConfirmedProposalState)
    , pmActiveProps    :: !(MapModifier UpId ProposalState)
    , pmSlottingData   :: !(Maybe SlottingData)
    , pmEpochProposers :: !(Maybe (HashSet StakeholderId))
    } deriving (Eq, Show, Generic)

flip makeLensesFor ''PollModifier
    [ ("pmBVs", "pmBVsL")
    , ("pmAdoptedBVFull", "pmAdoptedBVFullL")
    , ("pmConfirmed", "pmConfirmedL")
    , ("pmConfirmedProps", "pmConfirmedPropsL")
    , ("pmActiveProps", "pmActivePropsL")
    , ("pmSlottingData", "pmSlottingDataL")
    , ("pmEpochProposers", "pmEpochProposersL")
    ]

instance Default PollModifier where
    def =
        PollModifier
        { pmBVs = mempty
        , pmAdoptedBVFull = Nothing
        , pmConfirmed = mempty
        , pmConfirmedProps = mempty
        , pmActiveProps = mempty
        , pmSlottingData = Nothing
        , pmEpochProposers = mempty
        }

-- | Unite two PollModifiers. Second argument dominates, i. e. if
-- there are two confliciting modifications, the second one wins.
modifyPollModifier :: PollModifier -> PollModifier -> PollModifier
modifyPollModifier pmOld pmNew = PollModifier
    (pmBVs pmOld            <>  pmBVs pmNew)
    (pmAdoptedBVFull pmNew  <|> pmAdoptedBVFull pmOld)
    (pmConfirmed pmOld      <>  pmConfirmed pmNew)
    (pmConfirmedProps pmOld <>  pmConfirmedProps pmNew)
    (pmActiveProps pmOld    <>  pmActiveProps pmNew)
    (pmSlottingData pmNew   <|>  pmSlottingData pmOld)
    (pmEpochProposers pmNew <|> pmEpochProposers pmOld)


instance Semigroup PollModifier where

instance Monoid PollModifier where
    mempty = def
    mappend = modifyPollModifier
