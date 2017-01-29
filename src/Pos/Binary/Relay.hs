{-# LANGUAGE BangPatterns #-}

-- | Pos.Util.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Crypto                       (hash)
import           Pos.Ssc.GodTossing.Core.Types    (VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..))
import           Pos.Txp.Types.Communication      (TxMsgContents (..))
import           Pos.Types                        (TxId)
import           Pos.Types.Address                (StakeholderId, addressHash)
import           Pos.Update.Core                  (UpId, UpdateProposal, UpdateVote (..),
                                                   VoteId)
import           Pos.Util.Relay                   (DataMsg (..), InvMsg (..), ReqMsg (..))

instance (Bi tag, Bi key) => Bi (InvMsg key tag) where
    put InvMsg {..} = put imTag >> put imKeys
    get = liftM2 InvMsg get get

instance (Bi tag, Bi key) => Bi (ReqMsg key tag) where
    put ReqMsg {..} = put rmTag >> put rmKeys
    get = liftM2 ReqMsg get get

instance Bi (DataMsg StakeholderId GtMsgContents) where
    put DataMsg {..} = do
        put dmContents
        case dmContents of
            MCShares _         -> put dmKey
            MCOpening _        -> put dmKey
            MCCommitment _     -> pass
            MCVssCertificate _ -> pass
    get = do
        dmContents <- get
        dmKey <-
            case dmContents of
                MCCommitment (pk, _, _) -> pure $ addressHash pk
                MCVssCertificate vc     -> pure $ addressHash $ vcSigningKey vc
                _                       -> get
        return $ DataMsg {..}

instance Bi (DataMsg TxId TxMsgContents) where
    put (DataMsg (TxMsgContents dmTx dmWitness dmDistr) _) =
        put dmTx >> put dmWitness >> put dmDistr
    get = do
      tx <- get
      conts <- TxMsgContents tx <$> get <*> get
      pure $ DataMsg conts (hash tx)

instance Bi (DataMsg UpId (UpdateProposal, [UpdateVote])) where
    put DataMsg {..} = put dmContents
    get = do
        c@(up, votes) <- get
        let !id = hash up
        unless (all ((id ==) . uvProposalId) votes) $
            fail "get@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c id

instance Bi (DataMsg VoteId UpdateVote) where
    put (DataMsg uv _) = put uv
    get = do
        uv@UpdateVote{..} <- get
        pure $ DataMsg uv (uvProposalId, uvKey, uvDecision)
