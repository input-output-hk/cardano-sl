{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Communication.Message
       (
       ) where

import           Universum
import           Data.Tagged                      (Tagged)

import           Node.Message.Class               (Message (..))

import           Pos.Block.Network.Types          (MsgBlock, MsgGetBlocks, MsgGetHeaders,
                                                   MsgHeaders)
import           Pos.Communication.Types.Relay    (DataMsg, InvMsg, InvOrData, MempoolMsg,
                                                   ReqMsg)
import           Pos.Communication.Types.Protocol (MsgSubscribe)
import           Pos.Delegation.Types             (ProxySKLightConfirmation)
import           Pos.Ssc.GodTossing.Types.Message (MCCommitment, MCOpening, MCShares,
                                                   MCVssCertificate)
import           Pos.Txp.Network.Types            (TxMsgContents)
import           Pos.Types                        (ProxySKHeavy, ProxySKLight)
import           Pos.Update.Core.Types            (UpdateProposal, UpdateVote)

-- Why?
instance Message Void where
    messageCode _ = 0
    formatMessage _ = "Void"

instance Message MsgGetHeaders where
    messageCode _ = 4
    formatMessage _ = "GetHeaders"

instance Message (MsgHeaders ssc) where
    messageCode _ = 5
    formatMessage _ = "BlockHeaders"

instance Message MsgGetBlocks where
    messageCode _ = 6
    formatMessage _ = "GetBlocks"

instance Message (MsgBlock ssc) where
    messageCode _ = 7
    formatMessage _ = "Block"

instance Message MsgSubscribe where
    messageCode _ = 13
    formatMessage _ = "Subscribe"


instance Message k => Message (ReqMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged"

instance Message k => Message (MempoolMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged"

instance Message k => Message (DataMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged"

instance Message k => Message (InvMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged"

instance Message k => Message (InvOrData key (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged"


instance Message (ReqMsg TxMsgContents) where
    messageCode _ = 32
    formatMessage _ = "Request"

instance Message (MempoolMsg TxMsgContents) where
    messageCode _ = 33
    formatMessage _ = "Mempool"

instance Message (DataMsg TxMsgContents) where
    messageCode _ = 34
    formatMessage _ = "Data"

instance Message (InvMsg TxMsgContents) where
    messageCode _ = 35
    formatMessage _ = "Inventory"

instance Message (InvOrData key TxMsgContents) where
    messageCode _ = 36



instance Message (ReqMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 37
    formatMessage _ = "Request"

instance Message (MempoolMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 38
    formatMessage _ = "Mempool"

instance Message (DataMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 39
    formatMessage _ = "Data"

instance Message (InvMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 40
    formatMessage _ = "Inventory"

instance Message (InvOrData key (UpdateProposal, [UpdateVote])) where
    messageCode _ = 41
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg UpdateVote) where
    messageCode _ = 42
    formatMessage _ = "Request"

instance Message (MempoolMsg UpdateVote) where
    messageCode _ = 43
    formatMessage _ = "Mempool"

instance Message (DataMsg UpdateVote) where
    messageCode _ = 44
    formatMessage _ = "Data"

instance Message (InvMsg UpdateVote) where
    messageCode _ = 45
    formatMessage _ = "Inventory"

instance Message (InvOrData key UpdateVote) where
    messageCode _ = 46
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg MCCommitment) where
    messageCode _ = 47
    formatMessage _ = "Request"

instance Message (MempoolMsg MCCommitment) where
    messageCode _ = 48
    formatMessage _ = "Mempool"

instance Message (DataMsg MCCommitment) where
    messageCode _ = 49
    formatMessage _ = "Data"

instance Message (InvMsg MCCommitment) where
    messageCode _ = 50
    formatMessage _ = "Inventory"

instance Message (InvOrData key MCCommitment) where
    messageCode _ = 51
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg MCOpening) where
    messageCode _ = 52
    formatMessage _ = "Request"

instance Message (MempoolMsg MCOpening) where
    messageCode _ = 53
    formatMessage _ = "Mempool"

instance Message (DataMsg MCOpening) where
    messageCode _ = 54
    formatMessage _ = "Data"

instance Message (InvMsg MCOpening) where
    messageCode _ = 55
    formatMessage _ = "Inventory"

instance Message (InvOrData key MCOpening) where
    messageCode _ = 56
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg MCShares) where
    messageCode _ = 57
    formatMessage _ = "Request"

instance Message (MempoolMsg MCShares) where
    messageCode _ = 58
    formatMessage _ = "Mempool"

instance Message (DataMsg MCShares) where
    messageCode _ = 59
    formatMessage _ = "Data"

instance Message (InvMsg MCShares) where
    messageCode _ = 60
    formatMessage _ = "Inventory"

instance Message (InvOrData key MCShares) where
    messageCode _ = 61
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg MCVssCertificate) where
    messageCode _ = 62
    formatMessage _ = "Request"

instance Message (MempoolMsg MCVssCertificate) where
    messageCode _ = 63
    formatMessage _ = "Mempool"

instance Message (DataMsg MCVssCertificate) where
    messageCode _ = 64
    formatMessage _ = "Data"

instance Message (InvMsg MCVssCertificate) where
    messageCode _ = 65
    formatMessage _ = "Inventory"

instance Message (InvOrData key MCVssCertificate) where
    messageCode _ = 66
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg ProxySKLight) where
    messageCode _ = 67
    formatMessage _ = "Request"

instance Message (MempoolMsg ProxySKLight) where
    messageCode _ = 68
    formatMessage _ = "Mempool"

instance Message (DataMsg ProxySKLight) where
    messageCode _ = 69
    formatMessage _ = "Data"

instance Message (InvMsg ProxySKLight) where
    messageCode _ = 70
    formatMessage _ = "Inventory"

instance Message (InvOrData key ProxySKLight) where
    messageCode _ = 71
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg ProxySKHeavy) where
    messageCode _ = 72
    formatMessage _ = "Request"

instance Message (MempoolMsg ProxySKHeavy) where
    messageCode _ = 73
    formatMessage _ = "Mempool"

instance Message (DataMsg ProxySKHeavy) where
    messageCode _ = 74
    formatMessage _ = "Data"

instance Message (InvMsg ProxySKHeavy) where
    messageCode _ = 75
    formatMessage _ = "Inventory"

instance Message (InvOrData key ProxySKHeavy) where
    messageCode _ = 76
    formatMessage _ = "Inventory/Data"


instance Message (ReqMsg ProxySKLightConfirmation) where
    messageCode _ = 77
    formatMessage _ = "Request"

instance Message (MempoolMsg ProxySKLightConfirmation) where
    messageCode _ = 78
    formatMessage _ = "Mempool"

instance Message (DataMsg ProxySKLightConfirmation) where
    messageCode _ = 79
    formatMessage _ = "Data"

instance Message (InvMsg ProxySKLightConfirmation) where
    messageCode _ = 80
    formatMessage _ = "Inventory"

instance Message (InvOrData key ProxySKLightConfirmation) where
    messageCode _ = 81
    formatMessage _ = "Inventory/Data"


instance Message UpdateVote where
    messageCode _ = 82
    formatMessage _ = "UpdateVote"

instance Message (UpdateProposal, [UpdateVote]) where
    messageCode _ = 83
    formatMessage _ = "UpdateProposal"

instance Message TxMsgContents where
    messageCode _ = 84
    formatMessage _ = "TxMsgContents"

instance Message MCVssCertificate where
    messageCode _ = 85
    formatMessage _ = "MCVssCertificate"

instance Message MCShares where
    messageCode _ = 86
    formatMessage _ = "MCShares"

instance Message MCOpening where
    messageCode _ = 87
    formatMessage _ = "MCOpening"

instance Message MCCommitment where
    messageCode _ = 88
    formatMessage _ = "MCCommitment"
