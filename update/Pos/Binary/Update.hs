-- | 'Bi' instances for various types from cardano-sl-update.
module Pos.Binary.Update
       (
       ) where

import           Universum

import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class           (Bi (..), Cons (..), Field (..), Raw,
                                             dcNocheck, decodeListLenCanonical,
                                             deriveSimpleBi, deriveSimpleBiCxt,
                                             encodeListLen, enforceSize)
import           Pos.Binary.Infra           ()
import           Pos.Core                   (ApplicationName, BlockVersion,
                                             ChainDifficulty, Coin, CoinPortion,
                                             EpochIndex, FlatSlotId, HasConfiguration,
                                             HeaderHash, NumSoftwareVersion,
                                             ScriptVersion, SlotId, SoftforkRule,
                                             SoftwareVersion, StakeholderId, TxFeePolicy)
import           Pos.Crypto                 (Hash, SignTag (SignUSVote), checkSig)
import           Pos.Slotting.Types         (SlottingData)
import qualified Pos.Update.Core.Types      as U
import qualified Pos.Update.Poll.Types      as U

instance Bi U.SystemTag where
    encode = encode . U.getSystemTag
    decode = do
        tag <- decode
        ifM (view dcNocheck)
            (pure $ U.UnsafeSystemTag tag)
            (U.mkSystemTag tag)

instance HasConfiguration => Bi U.UpdateVote where
    encode uv =  encodeListLen 4
              <> encode (U.uvKey uv)
              <> encode (U.uvProposalId uv)
              <> encode (U.uvDecision uv)
              <> encode (U.uvSignature uv)
    decode = do
        enforceSize "UpdateVote" 4
        k <- decode
        p <- decode
        d <- decode
        s <- decode
        noCheck <- view dcNocheck
        let sigValid = checkSig SignUSVote k (p, d) s
        unless (noCheck || sigValid) $ fail "Pos.Binary.Update: UpdateVote: invalid signature"
        return $ U.UpdateVote k p d s

deriveSimpleBi ''U.UpdateData [
    Cons 'U.UpdateData [
        Field [| U.udAppDiffHash  :: Hash Raw |],
        Field [| U.udPkgHash      :: Hash Raw |],
        Field [| U.udUpdaterHash  :: Hash Raw |],
        Field [| U.udMetadataHash :: Hash Raw |]
    ]]

deriveSimpleBi ''U.BlockVersionModifier [
    Cons 'U.BlockVersionModifier [
        Field [| U.bvmScriptVersion     :: Maybe ScriptVersion |],
        Field [| U.bvmSlotDuration      :: Maybe Millisecond   |],
        Field [| U.bvmMaxBlockSize      :: Maybe Byte          |],
        Field [| U.bvmMaxHeaderSize     :: Maybe Byte          |],
        Field [| U.bvmMaxTxSize         :: Maybe Byte          |],
        Field [| U.bvmMaxProposalSize   :: Maybe Byte          |],
        Field [| U.bvmMpcThd            :: Maybe CoinPortion   |],
        Field [| U.bvmHeavyDelThd       :: Maybe CoinPortion   |],
        Field [| U.bvmUpdateVoteThd     :: Maybe CoinPortion   |],
        Field [| U.bvmUpdateProposalThd :: Maybe CoinPortion   |],
        Field [| U.bvmUpdateImplicit    :: Maybe FlatSlotId    |],
        Field [| U.bvmSoftforkRule      :: Maybe SoftforkRule  |],
        Field [| U.bvmTxFeePolicy       :: Maybe TxFeePolicy   |],
        Field [| U.bvmUnlockStakeEpoch  :: Maybe EpochIndex    |]
    ]]

instance HasConfiguration => Bi U.UpdateProposal where
    encode up =  encodeListLen 7
              <> encode (U.upBlockVersion up)
              <> encode (U.upBlockVersionMod up)
              <> encode (U.upSoftwareVersion up)
              <> encode (U.upData up)
              <> encode (U.upAttributes up)
              <> encode (U.upFrom up)
              <> encode (U.upSignature up)
    decode = do
        enforceSize "UpdateProposal" 7

        upBlockVersion    <- decode
        upBlockVersionMod <- decode
        upSoftwareVersion <- decode
        upData            <- decode
        upAttributes      <- decode
        upFrom            <- decode
        upSignature       <- decode

        ifM (view dcNocheck)
            (pure $ U.UnsafeUpdateProposal {..})
            (U.mkUpdateProposal
                 upBlockVersion
                 upBlockVersionMod
                 upSoftwareVersion
                 upData
                 upAttributes
                 upFrom
                 upSignature)

deriveSimpleBi ''U.UpdateProposalToSign [
    Cons 'U.UpdateProposalToSign [
        Field [| U.upsBV   :: BlockVersion                     |],
        Field [| U.upsBVM  :: U.BlockVersionModifier           |],
        Field [| U.upsSV   :: SoftwareVersion                  |],
        Field [| U.upsData :: HashMap U.SystemTag U.UpdateData |],
        Field [| U.upsAttr :: U.UpAttributes                   |]
    ]]

deriveSimpleBiCxt [t|HasConfiguration|] ''U.UpdatePayload [
    Cons 'U.UpdatePayload [
        Field [| U.upProposal :: Maybe U.UpdateProposal |],
        Field [| U.upVotes    :: [U.UpdateVote]         |]
    ]]

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
            _ -> fail $ "decode@PrevValue: invalid len: " <> show len

deriveSimpleBiCxt [t|HasConfiguration|] ''U.USUndo [
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

deriveSimpleBiCxt [t|HasConfiguration|] ''U.UndecidedProposalState [
    Cons 'U.UndecidedProposalState [
        Field [| U.upsVotes         :: U.StakeholderVotes |],
        Field [| U.upsProposal      :: U.UpdateProposal   |],
        Field [| U.upsSlot          :: SlotId             |],
        Field [| U.upsPositiveStake :: Coin               |],
        Field [| U.upsNegativeStake :: Coin               |],
        Field [| U.upsExtra         :: Maybe U.UpsExtra   |]
    ]]

deriveSimpleBiCxt [t|HasConfiguration|] ''U.DecidedProposalState [
    Cons 'U.DecidedProposalState [
        Field [| U.dpsDecision   :: Bool                     |],
        Field [| U.dpsUndecided  :: U.UndecidedProposalState |],
        Field [| U.dpsDifficulty :: Maybe ChainDifficulty    |],
        Field [| U.dpsExtra      :: Maybe U.DpsExtra         |]
    ]]

deriveSimpleBiCxt [t|HasConfiguration|] ''U.ProposalState [
    Cons 'U.PSUndecided [
        Field [| U.unPSUndecided :: U.UndecidedProposalState |]
    ],
    Cons 'U.PSDecided [
        Field [| U.unPSDecided :: U.DecidedProposalState |]
    ]]

deriveSimpleBiCxt [t|HasConfiguration|] ''U.ConfirmedProposalState [
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
