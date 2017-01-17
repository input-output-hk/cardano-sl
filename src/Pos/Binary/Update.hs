{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Update.Types module

module Pos.Binary.Update () where

import           Data.Binary           (Binary)
import           Data.Binary.Get       (getWord8, label)
import           Data.Binary.Put       (putWord8)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T
import           Universum

import           Pos.Binary.Class      (Bi (..))
import           Pos.Binary.Types      ()
import           Pos.Binary.Util       (getAsciiString1b, putAsciiString1b)
import           Pos.Binary.Version    ()
import           Pos.Crypto            (checkSig)
import qualified Pos.Update.Core.Types as U
import qualified Pos.Update.Poll.Types as U

instance Bi U.SystemTag where
    get =
        label "SystemTag" $
        U.mkSystemTag =<< T.pack <$> getAsciiString1b "SystemTag" U.systemTagMaxLength
    put (T.unpack . U.getSystemTag -> tag) = putAsciiString1b tag

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
    get = label "UpdateData" $ U.UpdateData <$> get <*> get <*> get
    put U.UpdateData {..} =  put udAppDiffHash
                          *> put udPkgHash
                          *> put udUpdaterHash

instance Bi U.UpdateProposal where
    get = label "UpdateProposal" $
          U.UpdateProposal <$> get <*> get <*> get <*> getUpData
      where getUpData = do   -- Check if proposal data is non-empty
                pd <- get
                when (HM.null pd) $
                    fail "Pos.Binary.Update: UpdateProposal: empty proposal data"
                return pd
    put U.UpdateProposal {..} =  put upProtocolVersion
                              *> put upScriptVersion
                              *> put upSoftwareVersion
                              *> put upData

instance Bi U.UpdatePayload where
    get = label "UpdatePayload" $ liftA2 U.UpdatePayload get get
    put U.UpdatePayload{..} =  put upProposal
                            *> put upVotes

-- These types are used only for DB. But it still makes sense to
-- define serialization manually I suppose.
-- [CSL-124]
instance Binary U.VoteState
instance Bi U.VoteState

instance Bi U.USUndo where
    get = pure U.USUndo
    put _ = pass

instance Bi U.UndecidedProposalState where
    put U.UndecidedProposalState {..} = do
        put upsVotes
        put upsProposal
        put upsSlot
        put upsPositiveStake
        put upsNegativeStake
    get = U.UndecidedProposalState <$> get <*> get <*> get <*> get <*> get

instance Bi U.DecidedProposalState where
    put U.DecidedProposalState {..} = do
        put dpsDecision
        put dpsProposal
        put dpsDifficulty
    get = U.DecidedProposalState <$> get <*> get <*> get

instance Bi U.ProposalState where
    put (U.PSUndecided us) = putWord8 0 >> put us
    put (U.PSDecided ds)   = putWord8 1 >> put ds
    get = getWord8 >>= \case
        0 -> U.PSUndecided <$> get
        1 -> U.PSDecided <$> get
        x -> fail $ "get@ProposalState: invalid tag: " <> show x
