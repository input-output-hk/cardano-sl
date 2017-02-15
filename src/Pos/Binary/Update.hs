{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Update.Types module

module Pos.Binary.Update () where

import           Data.Binary           (Binary)
import           Data.Binary.Get       (getWord8, label)
import           Data.Binary.Put       (putWord8)
import qualified Data.HashMap.Strict   as HM
import           Universum

import           Pos.Binary.Class      (Bi (..))
import           Pos.Binary.Types      ()
import           Pos.Binary.Version    ()
import           Pos.Crypto            (checkSig)
import qualified Pos.Update.Core.Types as U
import qualified Pos.Update.Poll.Types as U
import           Pos.Util.Binary       (getAsciiString1b, putAsciiString1b)

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
        unless (checkSig uvKey (uvProposalId, uvDecision) uvSignature) $
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

instance Bi U.BlockVersionData where
    get = label "BlockVersionData" $ do
        bvdScriptVersion     <- get
        bvdSlotDuration      <- get
        bvdMaxBlockSize      <- get
        bvdMaxTxSize         <- get
        bvdMpcThd            <- get
        bvdHeavyDelThd       <- get
        bvdUpdateVoteThd     <- get
        bvdUpdateProposalThd <- get
        bvdUpdateImplicit    <- get
        bvdUpdateSoftforkThd <- get
        return $ U.BlockVersionData {..}
    put U.BlockVersionData {..} = do
        put bvdScriptVersion
        put bvdSlotDuration
        put bvdMaxBlockSize
        put bvdMaxTxSize
        put bvdMpcThd
        put bvdHeavyDelThd
        put bvdUpdateVoteThd
        put bvdUpdateProposalThd
        put bvdUpdateImplicit
        put bvdUpdateSoftforkThd

instance Bi U.UpdateProposal where
    get = label "UpdateProposal" $
          U.UpdateProposal
            <$> get
            <*> get
            <*> get
            <*> getUpData
            <*> get
      where getUpData = do   -- Check if proposal data is non-empty
                pd <- get
                when (HM.null pd) $
                    fail "Pos.Binary.Update: UpdateProposal: empty proposal data"
                return pd
    put U.UpdateProposal {..} =  put upBlockVersion
                              *> put upBlockVersionData
                              *> put upSoftwareVersion
                              *> put upData
                              *> put upAttributes

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
        return $ U.USUndo {..}
    put U.USUndo{..} = do
        put unChangedBV
        put unLastAdoptedBV
        put unChangedProps
        put unChangedSV
        put unChangedConfProps

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
