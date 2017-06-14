{-# LANGUAGE TemplateHaskell #-}

-- | 'Bi' instances for various types from cardano-sl-update.
module Pos.Binary.Update
       (
       ) where

import           Universum

import           Pos.Binary.Class        (Bi (..), Cons (..), Field (..), PokeWithSize,
                                          Size (..), appendField, combineSize,
                                          convertSize, convertToSizeNPut, deriveSimpleBi,
                                          getAsciiString1b, getSize, getWord8, label,
                                          pokeWithSize, putAsciiString1b, putConst,
                                          putField, putWord8, putWord8S,
                                          sizeAsciiString1b)
import           Pos.Binary.Core         ()
import           Pos.Binary.Core.Version ()
import           Pos.Core.Types          (HeaderHash)
import           Pos.Crypto              (SignTag (SignUSVote), checkSig)
import qualified Pos.Update.Core.Types   as U
import qualified Pos.Update.Poll.Types   as U

-- TODO Most of Update types contains fields with composite types.
-- deriveSimpleBi doesn't support them yet.

instance Bi U.SystemTag where
    size = convertSize (toString . U.getSystemTag) sizeAsciiString1b
    put (toString . U.getSystemTag -> tag) = putAsciiString1b tag
    get =
        label "SystemTag" $
        U.mkSystemTag . toText =<< getAsciiString1b "SystemTag" U.systemTagMaxLength

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

-- TODO rewrite on deriveSimpleBi
instance Bi U.UpdateData where
    sizeNPut = putField U.udAppDiffHash
        `appendField` U.udPkgHash
        `appendField` U.udUpdaterHash
        `appendField` U.udMetadataHash
    get = label "UpdateData" $ U.UpdateData <$> get <*> get <*> get <*> get

-- TODO rewrite on deriveSimpleBi
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

-- TODO rewrite on deriveSimpleBi
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

-- TODO rewrite on deriveSimpleBi
instance Bi U.UpdatePayload where
    sizeNPut = putField U.upProposal <> putField U.upVotes
    get = label "UpdatePayload" $ liftA2 U.UpdatePayload get get

deriveSimpleBi ''U.VoteState [
    Cons 'U.PositiveVote [],
    Cons 'U.NegativeVote [],
    Cons 'U.PositiveRevote [],
    Cons 'U.NegativeRevote []]

instance Bi a => Bi (U.PrevValue a) where
    sizeNPut = convertToSizeNPut toBi
        where
          toBi :: Bi a => U.PrevValue a -> PokeWithSize ()
          toBi = \case
              U.PrevValue v -> putWord8S 2 <> pokeWithSize v
              U.NoExist     -> putWord8S 3
    get = label "PrevValue" $ getWord8 >>= \case
        2 -> U.PrevValue <$> get
        3 -> pure U.NoExist
        x -> fail $ "get@PrevValue: invalid tag: " <> show x

-- TODO rewrite on deriveSimpleBi
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

deriveSimpleBi ''U.UpsExtra [
    Cons 'U.UpsExtra [
        Field 'U.ueProposedBlk ''HeaderHash]]

deriveSimpleBi ''U.DpsExtra [
    Cons 'U.DpsExtra [
        Field 'U.deDecidedBlk ''HeaderHash,
        Field 'U.deImplicit ''Bool]]

-- TODO rewrite on deriveSimpleBi
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

-- TODO rewrite on deriveSimpleBi
instance Bi U.DecidedProposalState where
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

-- TODO rewrite on deriveSimpleBi
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

-- TODO rewrite on deriveSimpleBi
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
