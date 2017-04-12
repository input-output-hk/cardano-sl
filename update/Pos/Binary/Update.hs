-- | 'Bi' instances for various types from cardano-sl-update.
module Pos.Binary.Update
       (
       ) where

import           Universum

import           Data.Binary             (Binary)

import           Pos.Binary.Class        (Bi (..), getAsciiString1b, getWord8, label,
                                          putAsciiString1b, putWord8)
import           Pos.Binary.Core         ()
import           Pos.Binary.Core.Version ()
import           Pos.Crypto              (SignTag (SignUSVote), checkSig)
import qualified Pos.Update.Core.Types   as U
import qualified Pos.Update.Poll.Types   as U

instance Bi U.SystemTag where
    get =
        label "SystemTag" $
        U.mkSystemTag . toText =<< getAsciiString1b "SystemTag" U.systemTagMaxLength
    put (toString . U.getSystemTag -> tag) = putAsciiString1b tag

instance Bi U.UpdateVote where
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
    put U.UpdateVote {..} =  put uvKey
                          *> put uvProposalId
                          *> put uvDecision
                          *> put uvSignature

instance Bi U.UpdateData where
    get = label "UpdateData" $ U.UpdateData <$> get <*> get <*> get <*> get
    put U.UpdateData {..} =  put udAppDiffHash
                          *> put udPkgHash
                          *> put udUpdaterHash
                          *> put udMetadataHash

instance Bi U.UpdateProposal where
    get = label "UpdateProposal" $ do
        d <- get
        r <- get
        a <- get
        t <- get
        u <- get
        t' <- get
        i <- get
        U.mkUpdateProposal d r a t u t' i
    put U.UnsafeUpdateProposal {..} =  put upBlockVersion
                              *> put upBlockVersionData
                              *> put upSoftwareVersion
                              *> put upData
                              *> put upAttributes
                              *> put upFrom
                              *> put upSignature

instance Bi U.UpdateProposalToSign where
    get = label "UpdateProposalToSign" $
          U.UpdateProposalToSign
            <$> get
            <*> get
            <*> get
            <*> get
            <*> get
    put U.UpdateProposalToSign {..} = put upsBV *> put upsBVD *> put upsSV *> put upsData *> put upsAttr

instance Bi U.UpdatePayload where
    get = label "UpdatePayload" $ liftA2 U.UpdatePayload get get
    put U.UpdatePayload{..} =  put upProposal
                            *> put upVotes

-- These types are used only for DB. But it still makes sense to
-- define serialization manually I suppose.
-- [CSL-124]
instance Binary U.VoteState
instance Bi U.VoteState

instance Bi a => Bi (U.PrevValue a) where
    put (U.PrevValue v) = putWord8 2 >> put v
    put U.NoExist       = putWord8 3
    get = label "PrevValue" $ getWord8 >>= \case
        2 -> U.PrevValue <$> get
        3 -> pure U.NoExist
        x -> fail $ "get@PrevValue: invalid tag: " <> show x

instance Bi U.USUndo where
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
    put U.UpsExtra {..} = put ueProposedBlk
    get = label "UpsExtra" $ U.UpsExtra <$> get

instance Bi U.DpsExtra where
    put U.DpsExtra {..} = put deDecidedBlk *> put deImplicit
    get = label "DpsExtra" $ do
        deDecidedBlk <- get
        deImplicit <- get
        return $ U.DpsExtra {..}

instance Bi U.UndecidedProposalState where
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
    put (U.PSUndecided us) = putWord8 0 >> put us
    put (U.PSDecided ds)   = putWord8 1 >> put ds
    get = label "ProposalState" $ getWord8 >>= \case
        0 -> U.PSUndecided <$> get
        1 -> U.PSDecided <$> get
        x -> fail $ "get@ProposalState: invalid tag: " <> show x

--instance Binary U.ConfirmedProposalState
instance Bi U.ConfirmedProposalState where
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
