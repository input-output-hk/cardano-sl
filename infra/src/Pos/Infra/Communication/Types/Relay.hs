{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Infra.Communication.Types.Relay
       ( InvMsg (..)
       , ReqMsg (..)
       , ResMsg (..)
       , ReqOrRes
       , MempoolMsg (..)
       , DataMsg (..)
       , InvOrData
       , InvOrDataTK
       , RelayLogEvent (..)
       , SscMessageConstraints
       ) where

import           Universum hiding (id)

import           Control.Lens (Wrapped (..), iso)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Tagged (Tagged)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as B

import           Node.Message.Class (Message (..))
import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Core (ProxySKHeavy, StakeholderId)
import           Pos.Core.Txp (TxMsgContents (..))
import           Pos.Core.Update (UpdateProposal, UpdateVote, uvProposalId)
import           Pos.Crypto (hash)
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..),
                     MCShares (..), MCVssCertificate (..))
import           Pos.Util.Util (cborError)

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg key = InvMsg
    { imKey :: !key
    -- ^ Key for the data that you wish to announce.
    }
    deriving (Show, Eq)

instance Bi key => Bi (InvMsg key) where
    encode = encode . imKey
    decode = InvMsg <$> decode

instance Message k => Message (InvMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged InvMsg"

instance Message (InvMsg TxMsgContents) where
    messageCode _ = 36
    formatMessage _ = "Inventory"

instance Message (InvMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 42
    formatMessage _ = "Inventory"

instance Message (InvMsg UpdateVote) where
    messageCode _ = 48
    formatMessage _ = "Inventory"

instance Message (InvMsg MCCommitment) where
    messageCode _ = 54
    formatMessage _ = "Inventory"

instance Message (InvMsg MCOpening) where
    messageCode _ = 60
    formatMessage _ = "Inventory"

instance Message (InvMsg MCShares) where
    messageCode _ = 66
    formatMessage _ = "Inventory"

instance Message (InvMsg MCVssCertificate) where
    messageCode _ = 72
    formatMessage _ = "Inventory"

instance Message (InvMsg ProxySKHeavy) where
    messageCode _ = 84
    formatMessage _ = "Inventory"

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg key = ReqMsg
    { rmKey :: !(Maybe key)
    -- ^ Optional key for the data that you request.
    }
    deriving (Show, Eq)

instance Bi key => Bi (ReqMsg key) where
    encode = encode . rmKey
    decode = ReqMsg <$> decode

instance Message k => Message (ReqMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged ReqMsg"

instance Message (ReqMsg TxMsgContents) where
    messageCode _ = 32
    formatMessage _ = "Request"

instance Message (ReqMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 38
    formatMessage _ = "Request"

instance Message (ReqMsg UpdateVote) where
    messageCode _ = 44
    formatMessage _ = "Request"

instance Message (ReqMsg MCCommitment) where
    messageCode _ = 50
    formatMessage _ = "Request"

instance Message (ReqMsg MCOpening) where
    messageCode _ = 56
    formatMessage _ = "Request"

instance Message (ReqMsg MCShares) where
    messageCode _ = 62
    formatMessage _ = "Request"

instance Message (ReqMsg MCVssCertificate) where
    messageCode _ = 68
    formatMessage _ = "Request"

instance Message (ReqMsg ProxySKHeavy) where
    messageCode _ = 80
    formatMessage _ = "Request"

data MempoolMsg tag = MempoolMsg
    deriving (Show, Eq)

instance Message k => Message (MempoolMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged MempoolMsg"

instance Message (MempoolMsg TxMsgContents) where
    messageCode _ = 34
    formatMessage _ = "Mempool"

instance Message (MempoolMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 40
    formatMessage _ = "Mempool"

instance Message (MempoolMsg UpdateVote) where
    messageCode _ = 46
    formatMessage _ = "Mempool"

instance Message (MempoolMsg MCCommitment) where
    messageCode _ = 52
    formatMessage _ = "Mempool"

instance Message (MempoolMsg MCOpening) where
    messageCode _ = 58
    formatMessage _ = "Mempool"

instance Message (MempoolMsg MCShares) where
    messageCode _ = 64
    formatMessage _ = "Mempool"

instance Message (MempoolMsg MCVssCertificate) where
    messageCode _ = 70
    formatMessage _ = "Mempool"

instance Message (MempoolMsg ProxySKHeavy) where
    messageCode _ = 82
    formatMessage _ = "Mempool"

instance Typeable tag => Bi (MempoolMsg tag) where
    -- The extra byte is needed because time-warp doesn't work with
    -- possibly-empty messages. 228 was chosen as homage to @pva701
    encode MempoolMsg = encode (228 :: Word8)
    decode = do
        x <- decode @Word8
        when (x /= 228) $ cborError "wrong byte"
        pure MempoolMsg

-- | Data message. Can be used to send actual data.
data DataMsg contents = DataMsg
    { dmContents :: !contents
    } deriving (Generic, Show, Eq)

instance Bi (DataMsg ProxySKHeavy) where
    encode = encode . dmContents
    decode = DataMsg <$> decode

instance Bi (DataMsg TxMsgContents) where
    encode (DataMsg (TxMsgContents txAux)) = encode txAux
    decode = DataMsg <$> (TxMsgContents <$> decode)

instance Bi (DataMsg UpdateVote) where
    encode = encode . dmContents
    decode = DataMsg <$> decode

instance Bi (DataMsg (UpdateProposal, [UpdateVote])) where
    encode = encode . dmContents
    decode = do
        c@(up, votes) <- decode
        let !id = hash up
        -- FIXME don't do this in the decoder.
        unless (all ((id ==) . uvProposalId) votes) $ cborError $
            "decode@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

instance Message k => Message (DataMsg (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged DataMsg"

instance Message (DataMsg TxMsgContents) where
    messageCode _ = 35
    formatMessage _ = "Data"

instance Message (DataMsg (UpdateProposal, [UpdateVote])) where
    messageCode _ = 41
    formatMessage _ = "Data"

instance Message (DataMsg UpdateVote) where
    messageCode _ = 47
    formatMessage _ = "Data"

instance Message (DataMsg MCCommitment) where
    messageCode _ = 53
    formatMessage _ = "Data"

instance Message (DataMsg MCOpening) where
    messageCode _ = 59
    formatMessage _ = "Data"

instance Message (DataMsg MCShares) where
    messageCode _ = 65
    formatMessage _ = "Data"

instance Message (DataMsg MCVssCertificate) where
    messageCode _ = 71
    formatMessage _ = "Data"

instance Message (DataMsg ProxySKHeavy) where
    messageCode _ = 83
    formatMessage _ = "Data"

type InvOrData key contents = Either (InvMsg key) (DataMsg contents)

instance Message k => Message (InvOrData key (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged InvOrData"

instance Message (InvOrData key TxMsgContents) where
    messageCode _ = 37
    formatMessage _ = "TxMsgContents"

instance Message (InvOrData key (UpdateProposal, [UpdateVote])) where
    messageCode _ = 43
    formatMessage _ = "Inventory/Data"

instance Message (InvOrData key UpdateVote) where
    messageCode _ = 49
    formatMessage _ = "Inventory/Data"

instance Message (InvOrData key MCCommitment) where
    messageCode _ = 55
    formatMessage _ = "Inventory/Data"

instance Message (InvOrData key MCOpening) where
    messageCode _ = 61
    formatMessage _ = "Inventory/Data"

instance Message (InvOrData key MCShares) where
    messageCode _ = 67
    formatMessage _ = "Inventory/Data"

instance Message (InvOrData key MCVssCertificate) where
    messageCode _ = 73
    formatMessage _ = "Inventory/Data"

instance Message (InvOrData key ProxySKHeavy) where
    messageCode _ = 85
    formatMessage _ = "Inventory/Data"

-- | InvOrData with key tagged by contents
type InvOrDataTK key contents = InvOrData (Tagged contents key) contents

-- | Response to a 'ReqMsg' indicating whether it was successfully processed.
data ResMsg key = ResMsg
    { resKey :: !key
    -- ^ The key for the data to which this response is relevant (maybe it
    -- comes from a previous ReqMsg).
    , resOk  :: !Bool
    -- ^ True if the request was successfully processed.
    }
    deriving (Show, Eq)

instance Bi key => Bi (ResMsg key) where
    encode (ResMsg {..}) = encode (resKey, resOk)
    decode = uncurry ResMsg <$> decode

type ReqOrRes key = Either (ReqMsg key) (ResMsg key)

instance Message k => Message (ReqOrRes (Tagged k v)) where
    messageCode _ = messageCode (Proxy :: Proxy k)
    formatMessage _ = "Tagged ReqOrRes"

instance Message (ReqOrRes TxMsgContents) where
    messageCode _ = 33
    formatMessage _ = "ReqOrRes"

instance Message (ReqOrRes (UpdateProposal, [UpdateVote])) where
    messageCode _ = 39
    formatMessage _ = "ReqOrRes"

instance Message (ReqOrRes UpdateVote) where
    messageCode _ = 45
    formatMessage _ = "ReqOrRes"

instance Message (ReqOrRes MCCommitment) where
    messageCode _ = 51
    formatMessage _ = "ReqOrRes"

instance Message (ReqOrRes MCOpening) where
    messageCode _ = 57
    formatMessage _ = "ReqOrRes"

instance Message (ReqOrRes MCShares) where
    messageCode _ = 63
    formatMessage _ = "ReqOrRes"

instance Message (ReqOrRes MCVssCertificate) where
    messageCode _ = 69
    formatMessage _ = "ReqOrRes"

instance Message (ReqOrRes ProxySKHeavy) where
    messageCode _ = 81
    formatMessage _ = "ReqOrRes"

instance (Buildable contents) =>
         Buildable (DataMsg contents) where
    build (DataMsg contents) = bprint ("Data {" %build % "}") contents

instance Wrapped (DataMsg contents) where
    type Unwrapped (DataMsg contents) = contents
    _Wrapped' = iso dmContents DataMsg

data RelayLogEvent =
      RelayQueueFull
    | EnqueueDequeueTime !Integer
    deriving Show

instance Bi (DataMsg MCCommitment) where
    encode (DataMsg (MCCommitment signedComm)) = encode signedComm
    decode = DataMsg . MCCommitment <$> decode

instance Bi (DataMsg MCOpening) where
    encode (DataMsg (MCOpening sId opening)) = encodeListLen 2 <> encode sId <> encode opening
    decode = do
        enforceSize "DataMsg MCOpening" 2
        DataMsg <$> (MCOpening <$> decode <*> decode)

instance Bi (DataMsg MCShares) where
    encode (DataMsg (MCShares sId innerMap)) = encodeListLen 2 <> encode sId <> encode innerMap
    decode = do
        enforceSize "DataMsg MCShares" 2
        DataMsg <$> (MCShares <$> decode <*> decode)

instance Bi (DataMsg MCVssCertificate) where
    encode (DataMsg (MCVssCertificate vss)) = encode vss
    decode = DataMsg . MCVssCertificate <$> decode

-- TODO: someone who knows networking should take a look because this really
-- doesn't look like something that anyone should ever have to write
type SscMessageConstraints =
    ( Each '[Message]
        [ InvOrData (Tagged MCCommitment     StakeholderId) MCCommitment
        , InvOrData (Tagged MCOpening        StakeholderId) MCOpening
        , InvOrData (Tagged MCShares         StakeholderId) MCShares
        , InvOrData (Tagged MCVssCertificate StakeholderId) MCVssCertificate ]
    , Each '[Message]
        [ ReqMsg (Tagged MCCommitment     StakeholderId)
        , ReqMsg (Tagged MCOpening        StakeholderId)
        , ReqMsg (Tagged MCShares         StakeholderId)
        , ReqMsg (Tagged MCVssCertificate StakeholderId) ]
    , Each '[Message]
        [ ReqOrRes (Tagged MCCommitment     StakeholderId)
        , ReqOrRes (Tagged MCOpening        StakeholderId)
        , ReqOrRes (Tagged MCShares         StakeholderId)
        , ReqOrRes (Tagged MCVssCertificate StakeholderId) ]
    )

$(deriveJSON defaultOptions ''RelayLogEvent)
