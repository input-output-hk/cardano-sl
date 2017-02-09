module Pos.Communication.Message () where

import           Data.Proxy                       (Proxy (..))
import           Node.Message                     (Message (..),
                                                   MessageName (..))
import           Universum

import           Pos.Binary.Class                 (UnsignedVarInt (..),
                                                   encodeStrict)
import           Pos.Block.Network.Types          (MsgBlock, MsgGetBlocks,
                                                   MsgGetHeaders, MsgHeaders)
import           Pos.Communication.Types.Protocol (NOP)
import           Pos.Communication.Types.Relay    (DataMsg, InvMsg, ReqMsg)
import           Pos.Communication.Types.SysStart (SysStartRequest,
                                                   SysStartResponse)
import           Pos.Delegation.Types             (ConfirmProxySK, SendProxySK)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents, GtTag)
import           Pos.Txp.Types.Communication      (TxMsgContents, TxMsgTag)
import           Pos.Update.Core.Types            (UpdateProposal, UpdateVote)
import           Pos.Update.Network.Types         (ProposalMsgTag, VoteMsgTag)
import           Pos.Util.Binary                  (WithLengthLimited (..))

varIntMName :: Int -> MessageName
varIntMName = MessageName . encodeStrict . UnsignedVarInt

deriving instance Message a => Message (WithLengthLimited s a)

instance Message NOP where
    messageName _ = varIntMName 0
    formatMessage _ = "NOP"

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

instance (MessagePart tag) =>
         Message (InvMsg key tag) where
    messageName p = varIntMName 8 <> pMessageName (tagM p)
      where
        tagM :: Proxy (InvMsg key tag) -> Proxy tag
        tagM _ = Proxy
    formatMessage _ = "Inventory"

instance (MessagePart tag) =>
         Message (ReqMsg key tag) where
    messageName p = varIntMName 9 <> pMessageName (tagM p)
      where
        tagM :: Proxy (ReqMsg key tag) -> Proxy tag
        tagM _ = Proxy
    formatMessage _ = "Request"

instance (MessagePart contents) =>
         Message (DataMsg contents) where
    messageName p = varIntMName 10 <> pMessageName (contentsM p)
      where
        contentsM :: Proxy (DataMsg contents) -> Proxy contents
        contentsM _ = Proxy
    formatMessage _ = "Data"

class MessagePart a where
    pMessageName :: Proxy a -> MessageName

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

instance MessagePart GtTag where
    pMessageName _ = varIntMName 3

instance MessagePart GtMsgContents where
    pMessageName _ = varIntMName 3

instance Message SysStartRequest where
    messageName _ = varIntMName 1001
    formatMessage _ = "SysStartRequest"

instance Message SysStartResponse where
    messageName _ = varIntMName 1002
    formatMessage _ = "SysStartResponse"
