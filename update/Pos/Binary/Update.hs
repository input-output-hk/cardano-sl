-- | 'Bi' instances for various types from cardano-sl-update.
module Pos.Binary.Update
       (
       ) where

import           Universum

import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class           (Bi (..), Cons (..), Field (..), Raw,
                                             convertSize, convertToSizeNPut,
                                             deriveSimpleBi, getAsciiString1b, getWord8,
                                             label, labelP, labelS, putAsciiString1b,
                                             putField, putS, putWord8S, sizeAsciiString1b)
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
    size = convertSize (toString . U.getSystemTag) sizeAsciiString1b
    put (toString . U.getSystemTag -> tag) = labelP "SystemTag" $
        putAsciiString1b tag
    get = label "SystemTag" $
        U.mkSystemTag . toText =<< getAsciiString1b "SystemTag" U.systemTagMaxLength

instance Bi U.UpdateVote where
    sizeNPut = labelS "UpdateVote" $
        putField U.uvKey <>
        putField U.uvProposalId <>
        putField U.uvDecision <>
        putField U.uvSignature
    get = label "UpdateVote" $ do
        uvKey <- get
        uvProposalId <- get
        uvDecision <- get
        uvSignature <- get
        let sigValid = checkSig SignUSVote
                           uvKey
                           (uvProposalId, uvDecision)
                           uvSignature
        unless sigValid $
            fail "Pos.Binary.Update: UpdateVote: invalid signature"
        return U.UpdateVote {..}

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
    sizeNPut = labelS "UpdateProposal" $
        putField U.upBlockVersion <>
        putField U.upBlockVersionMod <>
        putField U.upSoftwareVersion <>
        putField U.upData <>
        putField U.upAttributes <>
        putField U.upFrom <>
        putField U.upSignature
    get = label "UpdateProposal" $ do
        d <- get
        r <- get
        a <- get
        t <- get
        u <- get
        t' <- get
        i <- get
        U.mkUpdateProposal d r a t u t' i

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
    sizeNPut = labelS "PrevValue" $
        convertToSizeNPut $ \case
            U.PrevValue v -> putWord8S 2 <> putS v
            U.NoExist     -> putWord8S 3
    get = label "PrevValue" $ getWord8 >>= \case
        2 -> U.PrevValue <$> get
        3 -> pure U.NoExist
        x -> fail $ "get@PrevValue: invalid tag: " <> show x

deriveSimpleBi ''U.USUndo [
    Cons 'U.USUndo [
        Field [| U.unChangedBV
                     :: HashMap BlockVersion
                          (U.PrevValue U.BlockVersionState)      |],
        Field [| U.unLastAdoptedBV
                     :: Maybe BlockVersion                       |],
        Field [| U.unChangedProps
                     :: HashMap U.UpId
                          (U.PrevValue U.ProposalState)          |],
        Field [| U.unChangedSV
                     :: HashMap ApplicationName
                          (U.PrevValue NumSoftwareVersion)       |],
        Field [| U.unChangedConfProps
                     :: HashMap SoftwareVersion
                          (U.PrevValue U.ConfirmedProposalState) |],
        Field [| U.unPrevProposers
                     :: Maybe (HashSet StakeholderId)            |],
        Field [| U.unSlottingData
                     :: Maybe SlottingData                       |]
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
        Field [| U.bvsIsConfirmed       :: Bool                   |],
        Field [| U.bvsIssuersStable     :: HashSet StakeholderId  |],
        Field [| U.bvsIssuersUnstable   :: HashSet StakeholderId  |],
        Field [| U.bvsLastBlockStable   :: Maybe HeaderHash       |],
        Field [| U.bvsLastBlockUnstable :: Maybe HeaderHash       |]
    ]]
