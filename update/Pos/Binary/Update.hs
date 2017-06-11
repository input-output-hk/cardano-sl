-- | 'Bi' instances for various types from cardano-sl-update.
module Pos.Binary.Update
       (
       ) where

import           Universum

import           Pos.Binary.Class        (Bi (..), Size (..), appendField, combineSize,
                                          convertSize, getAsciiString1b, getSize,
                                          getWord8, label, putAsciiString1b, putField,
                                          putWord8, sizeAsciiString1b)
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
    sizeNPut = putField U.uvKey
               `appendField` U.uvProposalId
               `appendField` U.uvDecision
               `appendField` U.uvSignature
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

instance Bi U.UpdateData where
    sizeNPut = putField U.udAppDiffHash
               `appendField` U.udPkgHash
               `appendField` U.udUpdaterHash
               `appendField` U.udMetadataHash
    get = label "UpdateData" $ U.UpdateData <$> get <*> get <*> get <*> get

instance Bi U.UpdateProposal where
    sizeNPut = putField U.upBlockVersion
        `appendField` U.upBlockVersionData
        `appendField` U.upSoftwareVersion
        `appendField` U.upData
        `appendField` U.upAttributes
        `appendField` U.upFrom
        `appendField` U.upSignature
    get = label "UpdateProposal" $ do
        d <- get
        r <- get
        a <- get
        t <- get
        u <- get
        t' <- get
        i <- get
        U.mkUpdateProposal d r a t u t' i

instance Bi U.UpdateProposalToSign where
    sizeNPut = putField U.upsBV
        `appendField` U.upsBVD
        `appendField` U.upsSV
        `appendField` U.upsData
        `appendField` U.upsAttr
    get = label "UpdateProposalToSign" $
          U.UpdateProposalToSign
            <$> get
            <*> get
            <*> get
            <*> get
            <*> get

instance Bi U.UpdatePayload where
    sizeNPut = putField U.upProposal <> putField U.upVotes
    get = label "UpdatePayload" $ liftA2 U.UpdatePayload get get

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
    sizeNPut =
        putField U.unChangedBV
        `appendField` U.unLastAdoptedBV
        `appendField` U.unChangedProps
        `appendField` U.unChangedSV
        `appendField` U.unChangedConfProps
        `appendField` U.unPrevProposers
    get = label "USUndo" $ do
        unChangedBV <- get
        unLastAdoptedBV <- get
        unChangedProps <- get
        unChangedSV <- get
        unChangedConfProps <- get
        unPrevProposers <- get
        return $ U.USUndo {..}

instance Bi U.UpsExtra where
    sizeNPut = putField U.ueProposedBlk
    get = label "UpsExtra" $ U.UpsExtra <$> get

instance Bi U.DpsExtra where
    sizeNPut = putField U.deDecidedBlk <> putField U.deImplicit
    get = label "DpsExtra" $ do
        deDecidedBlk <- get
        deImplicit <- get
        return $ U.DpsExtra {..}

instance Bi U.UndecidedProposalState where
    sizeNPut = putField U.upsVotes
        `appendField` U.upsProposal
        `appendField` U.upsSlot
        `appendField` U.upsPositiveStake
        `appendField` U.upsNegativeStake
        `appendField` U.upsExtra
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
    sizeNPut = putField U.dpsDecision
        `appendField` U.dpsUndecided
        `appendField` U.dpsDifficulty
        `appendField` U.dpsExtra
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
    sizeNPut = putField U.cpsUpdateProposal
        `appendField` U.cpsImplicit
        `appendField` U.cpsProposed
        `appendField` U.cpsDecided
        `appendField` U.cpsConfirmed
        `appendField` U.cpsAdopted
        `appendField` U.cpsVotes
        `appendField` U.cpsPositiveStake
        `appendField` U.cpsNegativeStake
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
    sizeNPut = putField U.bvsData
       `appendField` U.bvsIsConfirmed
       `appendField` U.bvsIssuersStable
       `appendField` U.bvsIssuersUnstable
       `appendField` U.bvsLastBlockStable
       `appendField` U.bvsLastBlockUnstable
    get = label "BlockVersionState" $ do
        bvsData <- get
        bvsIsConfirmed <- get
        bvsIssuersStable <- get
        bvsIssuersUnstable <- get
        bvsLastBlockStable <- get
        bvsLastBlockUnstable <- get
        return $ U.BlockVersionState {..}
