{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Communication.Message
       (
       ) where

import           Universum

import           Data.Tagged (Tagged)
import           Node.Message.Class (Message (..))

import           Pos.Block.Network (MsgBlock, MsgGetBlocks, MsgGetHeaders, MsgHeaders)
import           Pos.Communication.Types.Protocol (MsgSubscribe, MsgSubscribe1)
import           Pos.Communication.Types.Relay (DataMsg, InvMsg, InvOrData, MempoolMsg, ReqMsg,
                                                ReqOrRes)
import           Pos.Core (ProxySKHeavy)
import           Pos.Core.Update (UpdateProposal, UpdateVote)
import           Pos.Ssc.Message (MCCommitment, MCOpening, MCShares, MCVssCertificate)
import           Pos.Txp.Network.Types (TxMsgContents)

-- Why?
instance Message Void where
    messageCode _ = 0
    formatMessage _ = "Void"

instance Message MsgGetHeaders where
    messageCode _ = 4
    formatMessage _ = "GetHeaders"

instance Message MsgHeaders where
    messageCode _ = 5
    formatMessage _ = "BlockHeaders"

instance Message MsgGetBlocks where
    messageCode _ = 6
    formatMessage _ = "GetBlocks"

instance Message MsgBlock where
    messageCode _ = 7
    formatMessage _ = "Block"

instance Message MsgSubscribe1 where
    messageCode _ = 13
    formatMessage _ = "Subscribe1"

instance Message MsgSubscribe where
    messageCode _ = 14
    formatMessage _ = "Subscribe"

instance Message k => Message (ReqMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged ReqMsg"

instance Message k => Message (ReqOrRes (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged ReqOrRes"

instance Message k => Message (MempoolMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged MempoolMsg"

instance Message k => Message (DataMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged DataMsg"

instance Message k => Message (InvMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged InvMsg"

instance Message k => Message (InvOrData key (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged InvOrData"


instance Message (ReqMsg TxMsgContents) where
    messageCode _ = 32
    formatMessage _ = "Request"

instance Message (ReqOrRes TxMsgContents) where
    messageCode _ = 33
    formatMessage _ = "ReqOrRes"

instance Message (MempoolMsg TxMsgContents) where
    messageCode _ = 34
    formatMessage _ = "Mempool"

instance Message (DataMsg TxMsgContents) where
    messageCode _ = 35
    formatMessage _ = "Data"

instance Message (InvMsg TxMsgContents) where
    messageCode _ = 36
    formatMessage _ = "Inventory"

instance Message (InvOrData key TxMsgContents) where
    messageCode _ = 37
    formatMessage _ = "TxMsgContents"


instance Message (ReqMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 38
    formatMessage _ = "Request"

instance Message (ReqOrRes (UpdateProposal, [UpdateVote])) where
    messageCode _ = 39
    formatMessage _ = "ReqOrRes"

instance Message (MempoolMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 40
    formatMessage _ = "Mempool"

instance Message (DataMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 41
    formatMessage _ = "Data"

instance Message (InvMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 42
    formatMessage _ = "Inventory"

instance Message (InvOrData key (UpdateProposal, [UpdateVote])) where
    messageCode _ = 43
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg UpdateVote) where
    messageCode _ = 44
    formatMessage _ = "Request"

instance Message (ReqOrRes UpdateVote) where
    messageCode _ = 45
    formatMessage _ = "ReqOrRes"

instance Message (MempoolMsg UpdateVote) where
    messageCode _ = 46
    formatMessage _ = "Mempool"

instance Message (DataMsg UpdateVote) where
    messageCode _ = 47
    formatMessage _ = "Data"

instance Message (InvMsg UpdateVote) where
    messageCode _ = 48
    formatMessage _ = "Inventory"

instance Message (InvOrData key UpdateVote) where
    messageCode _ = 49
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg MCCommitment) where
    messageCode _ = 50
    formatMessage _ = "Request"

instance Message (ReqOrRes MCCommitment) where
    messageCode _ = 51
    formatMessage _ = "ReqOrRes"

instance Message (MempoolMsg MCCommitment) where
    messageCode _ = 52
    formatMessage _ = "Mempool"

instance Message (DataMsg MCCommitment) where
    messageCode _ = 53
    formatMessage _ = "Data"

instance Message (InvMsg MCCommitment) where
    messageCode _ = 54
    formatMessage _ = "Inventory"

instance Message (InvOrData key MCCommitment) where
    messageCode _ = 55
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg MCOpening) where
    messageCode _ = 56
    formatMessage _ = "Request"

instance Message (ReqOrRes MCOpening) where
    messageCode _ = 57
    formatMessage _ = "ReqOrRes"

instance Message (MempoolMsg MCOpening) where
    messageCode _ = 58
    formatMessage _ = "Mempool"

instance Message (DataMsg MCOpening) where
    messageCode _ = 59
    formatMessage _ = "Data"

instance Message (InvMsg MCOpening) where
    messageCode _ = 60
    formatMessage _ = "Inventory"

instance Message (InvOrData key MCOpening) where
    messageCode _ = 61
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg MCShares) where
    messageCode _ = 62
    formatMessage _ = "Request"

instance Message (ReqOrRes MCShares) where
    messageCode _ = 63
    formatMessage _ = "ReqOrRes"

instance Message (MempoolMsg MCShares) where
    messageCode _ = 64
    formatMessage _ = "Mempool"

instance Message (DataMsg MCShares) where
    messageCode _ = 65
    formatMessage _ = "Data"

instance Message (InvMsg MCShares) where
    messageCode _ = 66
    formatMessage _ = "Inventory"

instance Message (InvOrData key MCShares) where
    messageCode _ = 67
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg MCVssCertificate) where
    messageCode _ = 68
    formatMessage _ = "Request"

instance Message (ReqOrRes MCVssCertificate) where
    messageCode _ = 69
    formatMessage _ = "ReqOrRes"

instance Message (MempoolMsg MCVssCertificate) where
    messageCode _ = 70
    formatMessage _ = "Mempool"

instance Message (DataMsg MCVssCertificate) where
    messageCode _ = 71
    formatMessage _ = "Data"

instance Message (InvMsg MCVssCertificate) where
    messageCode _ = 72
    formatMessage _ = "Inventory"

instance Message (InvOrData key MCVssCertificate) where
    messageCode _ = 73
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg ProxySKHeavy) where
    messageCode _ = 80
    formatMessage _ = "Request"

instance Message (ReqOrRes ProxySKHeavy) where
    messageCode _ = 81
    formatMessage _ = "ReqOrRes"

instance Message (MempoolMsg ProxySKHeavy) where
    messageCode _ = 82
    formatMessage _ = "Mempool"

instance Message (DataMsg ProxySKHeavy) where
    messageCode _ = 83
    formatMessage _ = "Data"

instance Message (InvMsg ProxySKHeavy) where
    messageCode _ = 84
    formatMessage _ = "Inventory"

instance Message (InvOrData key ProxySKHeavy) where
    messageCode _ = 85
    formatMessage _ = "Inventory/Data"


instance Message UpdateVote where
    messageCode _ = 92
    formatMessage _ = "UpdateVote"

instance Message (UpdateProposal, [UpdateVote]) where
    messageCode _ = 93
    formatMessage _ = "UpdateProposal"

instance Message TxMsgContents where
    messageCode _ = 94
    formatMessage _ = "TxMsgContents"

instance Message MCVssCertificate where
    messageCode _ = 95
    formatMessage _ = "MCVssCertificate"

instance Message MCShares where
    messageCode _ = 96
    formatMessage _ = "MCShares"

instance Message MCOpening where
    messageCode _ = 97
    formatMessage _ = "MCOpening"

instance Message MCCommitment where
    messageCode _ = 98
    formatMessage _ = "MCCommitment"
