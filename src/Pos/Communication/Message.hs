module Pos.Communication.Message
       ( MessagePart (..)
       ) where

import           Universum

import           Node.Message                     (Message (..), MessageName (..))

import           Pos.Binary.Class                 (UnsignedVarInt (..), encodeStrict)
import           Pos.Block.Network.Types          (MsgBlock, MsgGetBlocks, MsgGetHeaders,
                                                   MsgHeaders)
import           Pos.Communication.MessagePart    (MessagePart (..))
import           Pos.Communication.Types.Relay    (DataMsg, InvMsg, InvOrData, MempoolMsg,
                                                   ReqMsg)
import           Pos.Delegation.Types             (ConfirmProxySK, SendProxySK)
import           Pos.Ssc.GodTossing.Types.Message (MCCommitment, MCOpening, MCShares,
                                                   MCVssCertificate)
import           Pos.Txp.Network.Types            (TxMsgContents, TxMsgTag)
import           Pos.Update.Core.Types            (UpdateProposal, UpdateVote)
import           Pos.Update.Network.Types         (ProposalMsgTag, VoteMsgTag)

varIntMName :: Int -> MessageName
varIntMName = MessageName . encodeStrict . UnsignedVarInt

instance Message SendProxySK where
    messageName _ = varIntMName 2
    formatMessage _ = "SendProxySK"

instance Message ConfirmProxySK where
    messageName _ = varIntMName 3
    formatMessage _ = "ConfirmProxySK"

--instance Message CheckProxySKConfirmed where
--    messageName _ = varIntMName _
--    formatMessage _ = "CheckProxySKConfirmed"
--
--instance Message CheckProxySKConfirmedRes where
--    messageName _ = varIntMName _
--    formatMessage _ = "CheckProxySKConfirmedRes"

instance Message MsgGetHeaders where
    messageName _ = varIntMName 4
    formatMessage _ = "GetHeaders"

instance Message (MsgHeaders ssc) where
    messageName _ = varIntMName 5
    formatMessage _ = "BlockHeaders"

instance Message MsgGetBlocks where
    messageName _ = varIntMName 6
    formatMessage _ = "GetBlocks"

instance Message (MsgBlock ssc) where
    messageName _ = varIntMName 7
    formatMessage _ = "Block"

instance MessagePart TxMsgTag where
    pMessageName _ = varIntMName 0

instance MessagePart TxMsgContents where
    pMessageName _ = varIntMName 0

instance MessagePart ProposalMsgTag where
    pMessageName _ = varIntMName 1

-- | Instance for `UpdateProposal`
instance MessagePart (UpdateProposal, [UpdateVote]) where
    pMessageName _ = varIntMName 1

instance MessagePart VoteMsgTag where
    pMessageName _ = varIntMName 2

-- | Instance for `UpdateVote`
instance MessagePart UpdateVote where
    pMessageName _ = varIntMName 2

instance MessagePart MCCommitment where
    pMessageName _ = varIntMName 3

instance MessagePart MCOpening where
    pMessageName _ = varIntMName 4

instance MessagePart MCShares where
    pMessageName _ = varIntMName 5

instance MessagePart MCVssCertificate where
    pMessageName _ = varIntMName 6

instance (MessagePart key) =>
         Message (ReqMsg key) where
    messageName p = varIntMName 9 <> pMessageName (keyM p)
      where
        keyM :: Proxy (ReqMsg key) -> Proxy key
        keyM _ = Proxy
    formatMessage _ = "Request"

instance (MessagePart tag) =>
         Message (MempoolMsg tag) where
    messageName p = varIntMName 10 <> pMessageName (tagM p)
      where
        tagM :: Proxy (MempoolMsg tag) -> Proxy tag
        tagM _ = Proxy
    formatMessage _ = "Mempool"

instance (MessagePart contents) =>
         Message (DataMsg contents) where
    messageName p = varIntMName 11 <> pMessageName (contentsM p)
      where
        contentsM :: Proxy (DataMsg contents) -> Proxy contents
        contentsM _ = Proxy
    formatMessage _ = "Data"

instance (MessagePart key) =>
         Message (InvMsg key) where
    messageName p = varIntMName 12 <> pMessageName (keyM p)
      where
        keyM :: Proxy (InvMsg key) -> Proxy key
        keyM _ = Proxy
    formatMessage _ = "Inventory"

instance (MessagePart contents) =>
         Message (InvOrData key contents) where
    messageName p = varIntMName 8 <>
                    pMessageName (contentsM p)
      where
        contentsM :: Proxy (InvOrData keys contents)
                  -> Proxy contents
        contentsM _ = Proxy
    formatMessage _ = "Inventory/Data"
