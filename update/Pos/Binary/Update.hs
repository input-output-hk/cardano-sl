-- | 'Bi' instances for various types from cardano-sl-update.
module Pos.Binary.Update
       (
       ) where

import           Universum

import           Pos.Binary.Class        (Bi (..), Size (..), combineSize, convertSize,
                                          getAsciiString1b, getSize, getWord8, label,
                                          putAsciiString1b, putWord8, sizeAddField,
                                          sizeAsciiString1b, sizeOf)
import           Pos.Binary.Core         ()
import           Pos.Binary.Core.Version ()
import           Pos.Crypto              (SignTag (SignUSVote), checkSig)
import qualified Pos.Update.Core.Types   as U
import qualified Pos.Update.Poll.Types   as U

instance Bi U.SystemTag where
    size = convertSize (toString . U.getSystemTag) sizeAsciiString1b
    get =
        label "SystemTag" $
        U.mkSystemTag . toText =<< getAsciiString1b "SystemTag" U.systemTagMaxLength
    put (toString . U.getSystemTag -> tag) = putAsciiString1b tag

instance Bi U.UpdateVote where
    size = combineSize (U.uvKey, U.uvProposalId, U.uvDecision, U.uvSignature)
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
    put U.UpdateVote {..} =
           put uvKey
        *> put uvProposalId
        *> put uvDecision
        *> put uvSignature

instance Bi U.UpdateData where
    size = combineSize (U.udAppDiffHash, U.udPkgHash, U.udUpdaterHash, U.udMetadataHash)
    get = label "UpdateData" $ U.UpdateData <$> get <*> get <*> get <*> get
    put U.UpdateData {..} =
           put udAppDiffHash
        *> put udPkgHash
        *> put udUpdaterHash
        *> put udMetadataHash

instance Bi U.UpdateProposal where
    size = ConstSize 0
        `sizeAddField` U.upBlockVersion
        `sizeAddField` U.upBlockVersionData
        `sizeAddField` U.upSoftwareVersion
        `sizeAddField` U.upData
        `sizeAddField` U.upAttributes
        `sizeAddField` U.upFrom
        `sizeAddField` U.upSignature
    get = label "UpdateProposal" $ do
        d <- get
        r <- get
        a <- get
        t <- get
        u <- get
        t' <- get
        i <- get
        U.mkUpdateProposal d r a t u t' i
    put U.UnsafeUpdateProposal {..} =
           put upBlockVersion
        *> put upBlockVersionData
        *> put upSoftwareVersion
        *> put upData
        *> put upAttributes
        *> put upFrom
        *> put upSignature

instance Bi U.UpdateProposalToSign where
    size = combineSize (U.upsBV, U.upsBVD, U.upsSV, U.upsData, U.upsAttr)
    get = label "UpdateProposalToSign" $
          U.UpdateProposalToSign
            <$> get
            <*> get
            <*> get
            <*> get
            <*> get
    put U.UpdateProposalToSign {..} =
           put upsBV
        *> put upsBVD
        *> put upsSV
        *> put upsData
        *> put upsAttr

instance Bi U.UpdatePayload where
    size = combineSize (U.upProposal, U.upVotes)
    get = label "UpdatePayload" $ liftA2 U.UpdatePayload get get
    put U.UpdatePayload{..} =
        put upProposal *> put upVotes

instance Bi U.VoteState where
    size = ConstSize 1
    get = label "VoteState" $ getWord8 >>= \case
        4 -> pure U.PositiveVote
        5 -> pure U.NegativeVote
        6 -> pure U.PositiveRevote
        7 -> pure U.NegativeRevote
        x -> fail $ "get@VoteState: invalid tag: " <> show x
    put = putWord8 . toByte
      where
        toByte = \case
            U.PositiveVote -> 4
            U.NegativeVote -> 5
            U.PositiveRevote -> 6
            U.NegativeRevote -> 7

instance Bi a => Bi (U.PrevValue a) where
    size = VarSize $ \case
        U.NoExist     -> 1
        U.PrevValue v -> 1 + getSize v
    put (U.PrevValue v) = putWord8 2 >> put v
    put U.NoExist       = putWord8 3
    get = label "PrevValue" $ getWord8 >>= \case
        2 -> U.PrevValue <$> get
        3 -> pure U.NoExist
        x -> fail $ "get@PrevValue: invalid tag: " <> show x

instance Bi U.USUndo where
    size = ConstSize 0
        `sizeAddField` U.unChangedBV
        `sizeAddField` U.unLastAdoptedBV
        `sizeAddField` U.unChangedProps
        `sizeAddField` U.unChangedSV
        `sizeAddField` U.unChangedConfProps
        `sizeAddField` U.unPrevProposers
    get = label "USUndo" $ do
        unChangedBV <- get
        unLastAdoptedBV <- get
        unChangedProps <- get
        unChangedSV <- get
        unChangedConfProps <- get
        unPrevProposers <- get
        return $ U.USUndo {..}
    put U.USUndo{..} = do
        put unChangedBV
        put unLastAdoptedBV
        put unChangedProps
        put unChangedSV
        put unChangedConfProps
        put unPrevProposers

instance Bi U.UpsExtra where
    size = sizeOf U.ueProposedBlk
    put U.UpsExtra {..} = put ueProposedBlk
    get = label "UpsExtra" $ U.UpsExtra <$> get

instance Bi U.DpsExtra where
    size = combineSize (U.deDecidedBlk, U.deImplicit)
    put U.DpsExtra {..} = put deDecidedBlk *> put deImplicit
    get = label "DpsExtra" $ do
        deDecidedBlk <- get
        deImplicit <- get
        return $ U.DpsExtra {..}

instance Bi U.UndecidedProposalState where
    size = ConstSize 0
        `sizeAddField` U.upsVotes
        `sizeAddField` U.upsProposal
        `sizeAddField` U.upsSlot
        `sizeAddField` U.upsPositiveStake
        `sizeAddField` U.upsNegativeStake
        `sizeAddField` U.upsExtra
    put U.UndecidedProposalState {..} = do
        put upsVotes
        put upsProposal
        put upsSlot
        put upsPositiveStake
        put upsNegativeStake
        put upsExtra
    get = label "UndecidedProposalState" $ do
        upsVotes <- get
        upsProposal <- get
        upsSlot <- get
        upsPositiveStake <- get
        upsNegativeStake <- get
        upsExtra <- get
        return $ U.UndecidedProposalState {..}

instance Bi U.DecidedProposalState where
    size = combineSize (U.dpsDecision, U.dpsUndecided, U.dpsDifficulty, U.dpsExtra)
    put U.DecidedProposalState {..} = do
        put dpsDecision
        put dpsUndecided
        put dpsDifficulty
        put dpsExtra
    get = label "DecidedProposalState" $ do
        dpsDecision <- get
        dpsUndecided <- get
        dpsDifficulty <- get
        dpsExtra <- get
        return $ U.DecidedProposalState {..}

instance Bi U.ProposalState where
    size = VarSize $ \case
        U.PSUndecided us -> 1 + getSize us
        U.PSDecided ds -> 1 + getSize ds
    put (U.PSUndecided us) = putWord8 0 >> put us
    put (U.PSDecided ds)   = putWord8 1 >> put ds
    get = label "ProposalState" $ getWord8 >>= \case
        0 -> U.PSUndecided <$> get
        1 -> U.PSDecided <$> get
        x -> fail $ "get@ProposalState: invalid tag: " <> show x

--instance Binary U.ConfirmedProposalState
instance Bi U.ConfirmedProposalState where
    size = ConstSize 0
        `sizeAddField` U.cpsUpdateProposal
        `sizeAddField` U.cpsImplicit
        `sizeAddField` U.cpsProposed
        `sizeAddField` U.cpsDecided
        `sizeAddField` U.cpsConfirmed
        `sizeAddField` U.cpsAdopted
        `sizeAddField` U.cpsVotes
        `sizeAddField` U.cpsPositiveStake
        `sizeAddField` U.cpsNegativeStake
    put U.ConfirmedProposalState {..} = do
        put cpsUpdateProposal
        put cpsImplicit
        put cpsProposed
        put cpsDecided
        put cpsConfirmed
        put cpsAdopted
        put cpsVotes
        put cpsPositiveStake
        put cpsNegativeStake
    get = label "ConfirmedProposalState" $ do
        cpsUpdateProposal <- get
        cpsImplicit <- get
        cpsProposed <- get
        cpsDecided <- get
        cpsConfirmed <- get
        cpsAdopted <- get
        cpsVotes <- get
        cpsPositiveStake <- get
        cpsNegativeStake <- get
        return $ U.ConfirmedProposalState {..}

instance Bi U.BlockVersionState where
    size = ConstSize 0
       `sizeAddField` U.bvsData
       `sizeAddField` U.bvsIsConfirmed
       `sizeAddField` U.bvsIssuersStable
       `sizeAddField` U.bvsIssuersUnstable
       `sizeAddField` U.bvsLastBlockStable
       `sizeAddField` U.bvsLastBlockUnstable
    put (U.BlockVersionState {..}) = do
        put bvsData
        put bvsIsConfirmed
        put bvsIssuersStable
        put bvsIssuersUnstable
        put bvsLastBlockStable
        put bvsLastBlockUnstable
    get = label "BlockVersionState" $ do
        bvsData <- get
        bvsIsConfirmed <- get
        bvsIssuersStable <- get
        bvsIssuersUnstable <- get
        bvsLastBlockStable <- get
        bvsLastBlockUnstable <- get
        return $ U.BlockVersionState {..}
