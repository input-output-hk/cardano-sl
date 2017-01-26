module Pos.Communication.Message () where

import           Data.Proxy                       (Proxy (..))
import           Node.Message                     (Message (..), MessageName (..))
import           Universum

import           Pos.Block.Network.Types          (MsgBlock, MsgGetBlocks, MsgGetHeaders,
                                                   MsgHeaders)
import           Pos.Communication.Types.Relay    (DataMsg, InvMsg, ReqMsg)
import           Pos.Communication.Types.SysStart (SysStartRequest, SysStartResponse)
import           Pos.Delegation.Types             (CheckProxySKConfirmed,
                                                   CheckProxySKConfirmedRes,
                                                   ConfirmProxySK, SendProxySK)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents, GtMsgTag)
import           Pos.Txp.Types.Communication      (TxMsgContents, TxMsgTag)
import           Pos.Update.Core.Types            (UpdateProposal, UpdateVote)
import           Pos.Update.Network.Types         (ProposalMsgTag, VoteMsgTag)

deriving instance Monoid (MessageName)

class MessagePart a where
    pMessageName :: Proxy a -> MessageName

instance Message SendProxySK where
    messageName _ = "SendProxySK"
    formatMessage _ = "SendProxySK"

instance Message ConfirmProxySK where
    messageName _ = "ConfirmProxySK"
    formatMessage _ = "ConfirmProxySK"

instance Message CheckProxySKConfirmed where
    messageName _ = "CheckProxySKConfirmed"
    formatMessage _ = "CheckProxySKConfirmed"

instance Message CheckProxySKConfirmedRes where
    messageName _ = "CheckProxySKConfirmedRes"
    formatMessage _ = "CheckProxySKConfirmedRes"

instance MessagePart TxMsgTag where
    pMessageName _     = "Tx tag"

instance MessagePart TxMsgContents where
    pMessageName _     = "Tx contents"

instance Message SysStartRequest where
    messageName _ = "SysStartRequest"
    formatMessage _ = "SysStartRequest"

instance Message SysStartResponse where
    messageName _ = "SysStartResponse"
    formatMessage _ = "SysStartResponse"

instance Message (MsgHeaders ssc) where
    messageName _ = "BlockHeaders"
    formatMessage _ = "BlockHeaders"

instance Message MsgGetHeaders where
    messageName _ = "GetHeaders"
    formatMessage _ = "GetHeaders"

instance Message MsgGetBlocks where
    messageName _ = "GetBlocks"
    formatMessage _ = "GetBlocks"

instance Message (MsgBlock s ssc) where
    messageName _ = "Block"
    formatMessage _ = "Block"

-- | Instance for `UpdateProposal`
instance MessagePart (UpdateProposal, [UpdateVote]) where
    pMessageName _ = "Update proposal with votes"

instance MessagePart ProposalMsgTag where
    pMessageName _ = "Update proposal tag"

instance MessagePart VoteMsgTag where
    pMessageName _ = "Update vote tag"

-- | Instance for `UpdateVote`
instance MessagePart UpdateVote where
    pMessageName _ = "Update vote"

instance MessagePart GtMsgTag where
    pMessageName _     = "Gt tag"

instance MessagePart GtMsgContents where
    pMessageName _     = "Gt contents"

instance (MessagePart tag) =>
         Message (InvMsg key tag) where
    messageName p = "Inventory " <> pMessageName (tagM p)
      where
        tagM :: Proxy (InvMsg key tag) -> Proxy tag
        tagM _ = Proxy
    formatMessage _ = "Inventory"

instance (MessagePart tag) =>
         Message (ReqMsg key tag) where
    messageName p = "Request " <> pMessageName (tagM p)
      where
        tagM :: Proxy (ReqMsg key tag) -> Proxy tag
        tagM _ = Proxy
    formatMessage _ = "Request"

instance (MessagePart contents) =>
         Message (DataMsg key contents) where
    messageName p = "Data " <> pMessageName (contentsM p)
      where
        contentsM :: Proxy (DataMsg key contents) -> Proxy contents
        contentsM _ = Proxy
    formatMessage _ = "Data"
