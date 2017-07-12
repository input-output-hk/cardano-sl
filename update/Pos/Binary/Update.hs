-- | 'Bi' instances for various types from cardano-sl-update.
module Pos.Binary.Update
       (
       ) where

import           Universum

import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import qualified Pos.Binary.Cbor            as Cbor
import           Pos.Binary.Class           (Bi (..), Cons (..), Field (..), Raw,
                                             convertSize, convertToSizeNPut,
                                             deriveSimpleBi, getAsciiString1b, getWord8,
                                             label, labelP, labelS, putAsciiString1b,
                                             putField, putS, putWord8S, sizeAsciiString1b)
import           Pos.Binary.Infra           ()
import           Pos.Core                   (ApplicationName, BlockVersion,
                                             ChainDifficulty, Coin, CoinPortion,
                                             FlatSlotId, HeaderHash, NumSoftwareVersion,
                                             ScriptVersion, SlotId, SoftwareVersion,
                                             StakeholderId, TxFeePolicy)
import           Pos.Crypto                 (Hash, SignTag (SignUSVote), Signature, checkSig)
import           Pos.Slotting.Types         (SlottingData)
import qualified Pos.Update.Core.Types      as U
import qualified Pos.Update.Poll.Types      as U

instance Bi U.SystemTag where
    size = convertSize (toString . U.getSystemTag) sizeAsciiString1b
    put (toString . U.getSystemTag -> tag) = labelP "SystemTag" $
        putAsciiString1b tag
    get = label "SystemTag" $
        U.mkSystemTag . toText =<< getAsciiString1b "SystemTag" U.systemTagMaxLength

instance Cbor.Bi U.SystemTag where
  encode = Cbor.encode . U.getSystemTag
  decode = Cbor.decode >>= \decoded -> case U.mkSystemTag decoded of
    Left e   -> fail e
    Right st -> pure st

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

instance Cbor.Bi U.UpdateVote where
  encode uv =  Cbor.encodeListLen 4
            <> Cbor.encode (U.uvKey uv)
            <> Cbor.encode (U.uvProposalId uv)
            <> Cbor.encode (U.uvDecision uv)
            <> Cbor.encode (U.uvSignature uv)
  decode = do
    Cbor.enforceSize "UpdateVote" 4
    k <- Cbor.decode
    p <- Cbor.decode
    d <- Cbor.decode
    s <- Cbor.decode
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

Cbor.deriveSimpleBi ''U.UpdateData [
    Cbor.Cons 'U.UpdateData [
        Cbor.Field [| U.udAppDiffHash  :: Hash Raw |],
        Cbor.Field [| U.udPkgHash      :: Hash Raw |],
        Cbor.Field [| U.udUpdaterHash  :: Hash Raw |],
        Cbor.Field [| U.udMetadataHash :: Hash Raw |]
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
        Field [| U.bvmUpdateSoftforkThd :: CoinPortion       |],
        Field [| U.bvmTxFeePolicy       :: Maybe TxFeePolicy |]
    ]]

Cbor.deriveSimpleBi ''U.BlockVersionModifier [
    Cbor.Cons 'U.BlockVersionModifier [
        Cbor.Field [| U.bvmScriptVersion     :: ScriptVersion     |],
        Cbor.Field [| U.bvmSlotDuration      :: Millisecond       |],
        Cbor.Field [| U.bvmMaxBlockSize      :: Byte              |],
        Cbor.Field [| U.bvmMaxHeaderSize     :: Byte              |],
        Cbor.Field [| U.bvmMaxTxSize         :: Byte              |],
        Cbor.Field [| U.bvmMaxProposalSize   :: Byte              |],
        Cbor.Field [| U.bvmMpcThd            :: CoinPortion       |],
        Cbor.Field [| U.bvmHeavyDelThd       :: CoinPortion       |],
        Cbor.Field [| U.bvmUpdateVoteThd     :: CoinPortion       |],
        Cbor.Field [| U.bvmUpdateProposalThd :: CoinPortion       |],
        Cbor.Field [| U.bvmUpdateImplicit    :: FlatSlotId        |],
        Cbor.Field [| U.bvmUpdateSoftforkThd :: CoinPortion       |],
        Cbor.Field [| U.bvmTxFeePolicy       :: Maybe TxFeePolicy |]
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

instance Cbor.Bi U.UpdateProposal where
  encode up =  Cbor.encodeListLen 7
            <> Cbor.encode (U.upBlockVersion up)
            <> Cbor.encode (U.upBlockVersionMod up)
            <> Cbor.encode (U.upSoftwareVersion up)
            <> Cbor.encode (U.upData up)
            <> Cbor.encode (U.upAttributes up)
            <> Cbor.encode (U.upFrom up)
            <> Cbor.encode (U.upSignature up)
  decode = do
    Cbor.enforceSize "UpdateProposal" 7
    up <- U.mkUpdateProposal <$> Cbor.decode
                             <*> Cbor.decode
                             <*> Cbor.decode
                             <*> Cbor.decode
                             <*> Cbor.decode
                             <*> Cbor.decode
                             <*> Cbor.decode
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

Cbor.deriveSimpleBi ''U.UpdateProposalToSign [
    Cbor.Cons 'U.UpdateProposalToSign [
        Cbor.Field [| U.upsBV   :: BlockVersion                     |],
        Cbor.Field [| U.upsBVM  :: U.BlockVersionModifier           |],
        Cbor.Field [| U.upsSV   :: SoftwareVersion                  |],
        Cbor.Field [| U.upsData :: HashMap U.SystemTag U.UpdateData |],
        Cbor.Field [| U.upsAttr :: U.UpAttributes                   |]
    ]]

deriveSimpleBi ''U.UpdatePayload [
    Cons 'U.UpdatePayload [
        Field [| U.upProposal :: Maybe U.UpdateProposal |],
        Field [| U.upVotes    :: [U.UpdateVote]         |]
    ]]

Cbor.deriveSimpleBi ''U.UpdatePayload [
    Cbor.Cons 'U.UpdatePayload [
        Cbor.Field [| U.upProposal :: Maybe U.UpdateProposal |],
        Cbor.Field [| U.upVotes    :: [U.UpdateVote]         |]
    ]]

deriveSimpleBi ''U.VoteState [
    Cons 'U.PositiveVote [],
    Cons 'U.NegativeVote [],
    Cons 'U.PositiveRevote [],
    Cons 'U.NegativeRevote []]

Cbor.deriveSimpleBi ''U.VoteState [
    Cbor.Cons 'U.PositiveVote [],
    Cbor.Cons 'U.NegativeVote [],
    Cbor.Cons 'U.PositiveRevote [],
    Cbor.Cons 'U.NegativeRevote []]

instance Bi a => Bi (U.PrevValue a) where
    sizeNPut = labelS "PrevValue" $
        convertToSizeNPut $ \case
            U.PrevValue v -> putWord8S 2 <> putS v
            U.NoExist     -> putWord8S 3
    get = label "PrevValue" $ getWord8 >>= \case
        2 -> U.PrevValue <$> get
        3 -> pure U.NoExist
        x -> fail $ "get@PrevValue: invalid tag: " <> show x

instance Cbor.Bi a => Cbor.Bi (U.PrevValue a) where
  encode (U.PrevValue a) = Cbor.encodeListLen 2 <> Cbor.encode (2 :: Word8) <> Cbor.encode a
  encode U.NoExist       = Cbor.encodeListLen 1 <> Cbor.encode (3 :: Word8)
  decode = do
    len <- Cbor.decodeListLen
    tag <- Cbor.decode @Word8
    case (len, tag) of
      (2,2) -> U.PrevValue <$> Cbor.decode
      (1,3) -> pure U.NoExist
      _     -> fail $ "decode@PrevValue: invalid tag: " <> show tag

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

Cbor.deriveSimpleBi ''U.USUndo [
    Cbor.Cons 'U.USUndo [
        Cbor.Field [| U.unChangedBV :: HashMap BlockVersion (U.PrevValue U.BlockVersionState)                |],
        Cbor.Field [| U.unLastAdoptedBV :: Maybe BlockVersion                                                |],
        Cbor.Field [| U.unChangedProps :: HashMap U.UpId (U.PrevValue U.ProposalState)                       |],
        Cbor.Field [| U.unChangedSV :: HashMap ApplicationName (U.PrevValue NumSoftwareVersion)              |],
        Cbor.Field [| U.unChangedConfProps :: HashMap SoftwareVersion (U.PrevValue U.ConfirmedProposalState) |],
        Cbor.Field [| U.unPrevProposers :: Maybe (HashSet StakeholderId)                                     |],
        Cbor.Field [| U.unSlottingData :: Maybe SlottingData                                                 |]
    ]]

deriveSimpleBi ''U.UpsExtra [
    Cons 'U.UpsExtra [
        Field [| U.ueProposedBlk :: HeaderHash |]
    ]]

Cbor.deriveSimpleBi ''U.UpsExtra [
    Cbor.Cons 'U.UpsExtra [
        Cbor.Field [| U.ueProposedBlk :: HeaderHash |]
    ]]

deriveSimpleBi ''U.DpsExtra [
    Cons 'U.DpsExtra [
        Field [| U.deDecidedBlk :: HeaderHash |],
        Field [| U.deImplicit   :: Bool       |]
    ]]

Cbor.deriveSimpleBi ''U.DpsExtra [
    Cbor.Cons 'U.DpsExtra [
        Cbor.Field [| U.deDecidedBlk :: HeaderHash |],
        Cbor.Field [| U.deImplicit   :: Bool       |]
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

Cbor.deriveSimpleBi ''U.UndecidedProposalState [
    Cbor.Cons 'U.UndecidedProposalState [
        Cbor.Field [| U.upsVotes         :: U.StakeholderVotes |],
        Cbor.Field [| U.upsProposal      :: U.UpdateProposal   |],
        Cbor.Field [| U.upsSlot          :: SlotId             |],
        Cbor.Field [| U.upsPositiveStake :: Coin               |],
        Cbor.Field [| U.upsNegativeStake :: Coin               |],
        Cbor.Field [| U.upsExtra         :: Maybe U.UpsExtra   |]
    ]]

deriveSimpleBi ''U.DecidedProposalState [
    Cons 'U.DecidedProposalState [
        Field [| U.dpsDecision   :: Bool                     |],
        Field [| U.dpsUndecided  :: U.UndecidedProposalState |],
        Field [| U.dpsDifficulty :: Maybe ChainDifficulty    |],
        Field [| U.dpsExtra      :: Maybe U.DpsExtra         |]
    ]]

Cbor.deriveSimpleBi ''U.DecidedProposalState [
    Cbor.Cons 'U.DecidedProposalState [
        Cbor.Field [| U.dpsDecision   :: Bool                     |],
        Cbor.Field [| U.dpsUndecided  :: U.UndecidedProposalState |],
        Cbor.Field [| U.dpsDifficulty :: Maybe ChainDifficulty    |],
        Cbor.Field [| U.dpsExtra      :: Maybe U.DpsExtra         |]
    ]]

deriveSimpleBi ''U.ProposalState [
    Cons 'U.PSUndecided [
        Field [| U.unPSUndecided :: U.UndecidedProposalState |]
    ],
    Cons 'U.PSDecided [
        Field [| U.unPSDecided :: U.DecidedProposalState |]
    ]]

Cbor.deriveSimpleBi ''U.ProposalState [
    Cbor.Cons 'U.PSUndecided [
        Cbor.Field [| U.unPSUndecided :: U.UndecidedProposalState |]
    ],
    Cbor.Cons 'U.PSDecided [
        Cbor.Field [| U.unPSDecided :: U.DecidedProposalState |]
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

Cbor.deriveSimpleBi ''U.ConfirmedProposalState [
    Cbor.Cons 'U.ConfirmedProposalState [
        Cbor.Field [| U.cpsUpdateProposal :: U.UpdateProposal   |],
        Cbor.Field [| U.cpsImplicit       :: Bool               |],
        Cbor.Field [| U.cpsProposed       :: HeaderHash         |],
        Cbor.Field [| U.cpsDecided        :: HeaderHash         |],
        Cbor.Field [| U.cpsConfirmed      :: HeaderHash         |],
        Cbor.Field [| U.cpsAdopted        :: Maybe HeaderHash   |],
        Cbor.Field [| U.cpsVotes          :: U.StakeholderVotes |],
        Cbor.Field [| U.cpsPositiveStake  :: Coin               |],
        Cbor.Field [| U.cpsNegativeStake  :: Coin               |]
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

Cbor.deriveSimpleBi ''U.BlockVersionState [
    Cbor.Cons 'U.BlockVersionState [
        Cbor.Field [| U.bvsModifier          :: U.BlockVersionModifier |],
        Cbor.Field [| U.bvsIsConfirmed       :: Bool                   |],
        Cbor.Field [| U.bvsIssuersStable     :: HashSet StakeholderId  |],
        Cbor.Field [| U.bvsIssuersUnstable   :: HashSet StakeholderId  |],
        Cbor.Field [| U.bvsLastBlockStable   :: Maybe HeaderHash       |],
        Cbor.Field [| U.bvsLastBlockUnstable :: Maybe HeaderHash       |]
    ]]
