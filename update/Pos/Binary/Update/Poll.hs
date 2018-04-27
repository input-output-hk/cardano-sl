module Pos.Binary.Update.Poll
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), decodeListLenCanonical,
                                   deriveSimpleBi, encodeListLen)
import           Pos.Binary.Infra ()
import           Pos.Core (ApplicationName, BlockVersion, ChainDifficulty, Coin, EpochIndex,
                           HeaderHash, NumSoftwareVersion, SlotId,
                           SoftwareVersion, StakeholderId)
import qualified Pos.Core.Update as U
import           Pos.Slotting.Types (SlottingData)
import qualified Pos.Update.Poll.Types as U
import           Pos.Util.Util (cborError)

deriveSimpleBi ''U.VoteState [
    Cons 'U.PositiveVote [],
    Cons 'U.NegativeVote [],
    Cons 'U.PositiveRevote [],
    Cons 'U.NegativeRevote []]

instance Bi a => Bi (U.PrevValue a) where
    encode (U.PrevValue a) = encodeListLen 1 <> encode a
    encode U.NoExist       = encodeListLen 0
    decode = do
        len <- decodeListLenCanonical
        case len of
            1 -> U.PrevValue <$> decode
            0 -> pure U.NoExist
            _ -> cborError $ "decode@PrevValue: invalid len: " <> show len

deriveSimpleBi ''U.USUndo [
    Cons 'U.USUndo [
        Field [| U.unChangedBV :: HashMap BlockVersion (U.PrevValue U.BlockVersionState)                |],
        Field [| U.unLastAdoptedBV :: Maybe BlockVersion                                                |],
        Field [| U.unChangedProps :: HashMap U.UpId (U.PrevValue U.ProposalState)                       |],
        Field [| U.unChangedSV :: HashMap ApplicationName (U.PrevValue NumSoftwareVersion)              |],
        Field [| U.unChangedConfProps :: HashMap SoftwareVersion (U.PrevValue U.ConfirmedProposalState) |],
        Field [| U.unPrevProposers :: Maybe (HashSet StakeholderId)                                     |],
        Field [| U.unSlottingData :: Maybe SlottingData                                                 |]
    ]]

deriveSimpleBi ''U.UpsExtra [
    Cons 'U.UpsExtra [
        Field [| U.ueProposedBlk :: HeaderHash |]
    ]]

deriveSimpleBi ''U.DpsExtra [
    Cons 'U.DpsExtra [
        Field [| U.deDecidedBlk :: HeaderHash |],
        Field [| U.deImplicit   :: Bool       |]
    ]]

deriveSimpleBi ''U.UndecidedProposalState [
    Cons 'U.UndecidedProposalState [
        Field [| U.upsVotes         :: U.StakeholderVotes |],
        Field [| U.upsProposal      :: U.UpdateProposal   |],
        Field [| U.upsSlot          :: SlotId             |],
        Field [| U.upsPositiveStake :: Coin               |],
        Field [| U.upsNegativeStake :: Coin               |],
        Field [| U.upsExtra         :: Maybe U.UpsExtra   |]
    ]]

deriveSimpleBi ''U.DecidedProposalState [
    Cons 'U.DecidedProposalState [
        Field [| U.dpsDecision   :: Bool                     |],
        Field [| U.dpsUndecided  :: U.UndecidedProposalState |],
        Field [| U.dpsDifficulty :: Maybe ChainDifficulty    |],
        Field [| U.dpsExtra      :: Maybe U.DpsExtra         |]
    ]]

deriveSimpleBi ''U.ProposalState [
    Cons 'U.PSUndecided [
        Field [| U.unPSUndecided :: U.UndecidedProposalState |]
    ],
    Cons 'U.PSDecided [
        Field [| U.unPSDecided :: U.DecidedProposalState |]
    ]]

deriveSimpleBi ''U.ConfirmedProposalState [
    Cons 'U.ConfirmedProposalState [
        Field [| U.cpsUpdateProposal :: U.UpdateProposal   |],
        Field [| U.cpsImplicit       :: Bool               |],
        Field [| U.cpsProposed       :: HeaderHash         |],
        Field [| U.cpsDecided        :: HeaderHash         |],
        Field [| U.cpsConfirmed      :: HeaderHash         |],
        Field [| U.cpsAdopted        :: Maybe HeaderHash   |],
        Field [| U.cpsVotes          :: U.StakeholderVotes |],
        Field [| U.cpsPositiveStake  :: Coin               |],
        Field [| U.cpsNegativeStake  :: Coin               |]
    ]]

deriveSimpleBi ''U.BlockVersionState [
    Cons 'U.BlockVersionState [
        Field [| U.bvsModifier          :: U.BlockVersionModifier |],
        Field [| U.bvsConfirmedEpoch    :: Maybe EpochIndex       |],
        Field [| U.bvsIssuersStable     :: HashSet StakeholderId  |],
        Field [| U.bvsIssuersUnstable   :: HashSet StakeholderId  |],
        Field [| U.bvsLastBlockStable   :: Maybe HeaderHash       |],
        Field [| U.bvsLastBlockUnstable :: Maybe HeaderHash       |]
    ]]
