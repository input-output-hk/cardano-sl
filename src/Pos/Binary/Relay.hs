-- | Pos.Util.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Crypto                       (hash)
import           Pos.Ssc.GodTossing.Types.Base    (VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..))
import           Pos.Txp.Types.Communication      (TxMsgContents (..))
import           Pos.Types                        (TxId)
import           Pos.Types.Address                (StakeholderId, addressHash)
import           Pos.Update.Core                  (UpId, UpdateProposal, UpdateVote (..),
                                                   VoteId)
import           Pos.Util.Relay                   (DataMsg (..), DataMsgGodTossing (..),
                                                   InvMsg (..), ReqMsg (..))

instance (Bi tag, Bi key) => Bi (InvMsg key tag) where
    put InvMsg {..} = put imTag >> put imKeys
    get = liftM2 InvMsg get get

instance (Bi tag, Bi key) => Bi (ReqMsg key tag) where
    put ReqMsg {..} = put rmTag >> put rmKeys
    get = liftM2 ReqMsg get get

-- Sometimes we want another instances to exist
--instance (Bi tag, Bi contents) => Bi (DataMsg key contents) where
--    put DataMsg {..} = put dmContents >> put dmKey
--    get = liftM2 DataMsg get get

instance Bi (DataMsg StakeholderId GtMsgContents) where
    put DataMsg {..} = put dmContents >> put dmKey
    get = do
        dmContents <- get
        dmKey <- get
        case dmContents of
            MCCommitment (pk, _, _) ->
                when (addressHash pk /= dmKey) $
                fail
                    "get@DataMsg@GodTossing: stakeholder ID doesn't correspond to public key from commitment"
            MCVssCertificate VssCertificate {..} ->
                when (addressHash vcSigningKey /= dmKey) $
                fail
                    "get@DataMsg@GodTossing: stakeholder ID doesn't correspond to public key from VSS certificate"
            _ -> pass
        return $ DataMsg {..}

instance Bi DataMsgGodTossing where
    put = put . getDataMsg
    get = DataMsgGT <$> get

instance Bi (DataMsg TxId TxMsgContents) where
    put (DataMsg (TxMsgContents dmTx dmWitness dmDistr) _) =
        put dmTx >> put dmWitness >> put dmDistr
    get = do
      tx <- get
      conts <- TxMsgContents tx <$> get <*> get
      pure $ DataMsg conts (hash tx)

instance Bi (DataMsg UpId UpdateProposal) where
    put DataMsg {..} = put dmContents >> put dmKey
    get = liftM2 DataMsg get get

instance Bi (DataMsg VoteId UpdateVote) where
    put (DataMsg uv _) = put uv
    get = do
        uv@UpdateVote{..} <- get
        pure $ DataMsg uv (uvProposalId, uvKey, uvDecision)
