-- | 'Bi' instances for various types from cardano-sl-update.
module Pos.Binary.Update
       (
       ) where

import           Universum

import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class           (Bi (..), Cons (..), Field (..), Raw, deriveSimpleBi, enforceSize,
                                             encodeListLen, decodeListLen)
import           Pos.Binary.Infra           ()
import           Pos.Core                   (ApplicationName, BlockVersion,
                                             ChainDifficulty, Coin, CoinPortion,
                                             EpochIndex, FlatSlotId, HeaderHash,
                                             NumSoftwareVersion, ScriptVersion, SlotId,
                                             SoftforkRule, SoftwareVersion, StakeholderId,
                                             TxFeePolicy)
import           Pos.Crypto                 (Hash, SignTag (SignUSVote), checkSig)
import           Pos.Slotting.Types         (SlottingData)
import qualified Pos.Update.Core.Types      as U
import qualified Pos.Update.Poll.Types      as U

instance Bi U.SystemTag where
  encode = encode . U.getSystemTag
  decode = decode >>= \decoded -> case U.mkSystemTag decoded of
    Left e   -> fail e
    Right st -> pure st

instance Bi U.UpdateVote where
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
    let sigValid = checkSig SignUSVote k (p, d) s
    unless sigValid $ fail "Pos.Binary.Update: UpdateVote: invalid signature"
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
        Field [| U.bvmScriptVersion     :: ScriptVersion     |],
        Field [| U.bvmSlotDuration      :: Millisecond       |],
        Field [| U.bvmMaxBlockSize      :: Byte              |],
        Field [| U.bvmMaxHeaderSize     :: Byte              |],
        Field [| U.bvmMaxTxSize         :: Byte              |],
        Field [| U.bvmMaxProposalSize   :: Byte              |],
        Field [| U.bvmMpcThd            :: CoinPortion       |],
        Field [| U.bvmHeavyDelThd       :: CoinPortion       |],
        Field [| U.bvmUpdateVoteThd     :: CoinPortion       |],
        Field [| U.bvmUpdateProposalThd :: CoinPortion       |],
        Field [| U.bvmUpdateImplicit    :: FlatSlotId        |],
        Field [| U.bvmSoftforkRule      :: Maybe SoftforkRule|],
        Field [| U.bvmTxFeePolicy       :: Maybe TxFeePolicy |],
        Field [| U.bvmUnlockStakeEpoch  :: Maybe EpochIndex  |]
    ]]

instance Bi U.UpdateProposal where
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
    up <- U.mkUpdateProposal <$> decode
                             <*> decode
                             <*> decode
                             <*> decode
                             <*> decode
                             <*> decode
                             <*> decode
    case up of
      Left e  -> fail e
      Right p -> pure p

deriveSimpleBi ''U.UpdateProposalToSign [
    Cons 'U.UpdateProposalToSign [
        Field [| U.upsBV   :: BlockVersion                     |],
        Field [| U.upsBVM  :: U.BlockVersionModifier           |],
        Field [| U.upsSV   :: SoftwareVersion                  |],
        Field [| U.upsData :: HashMap U.SystemTag U.UpdateData |],
        Field [| U.upsAttr :: U.UpAttributes                   |]
    ]]

deriveSimpleBi ''U.UpdatePayload [
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
  encode (U.PrevValue a) = encodeListLen 2 <> encode (2 :: Word8) <> encode a
  encode U.NoExist       = encodeListLen 1 <> encode (3 :: Word8)
  decode = do
    len <- decodeListLen
    tag <- decode @Word8
    case (len, tag) of
      (2,2) -> U.PrevValue <$> decode
      (1,3) -> pure U.NoExist
      _     -> fail $ "decode@PrevValue: invalid tag: " <> show tag

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
