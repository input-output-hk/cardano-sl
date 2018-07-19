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

import           Node.Message.Class (Message)
import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Core (ProxySKHeavy, StakeholderId)
import           Pos.Core.Txp (TxMsgContents (..))
import qualified Pos.Core.Update as U
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

data MempoolMsg tag = MempoolMsg
    deriving (Show, Eq)

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

instance Bi (DataMsg U.UpdateVote) where
    encode = encode . dmContents
    decode = DataMsg <$> decode

instance Bi (DataMsg (U.UpdateProposal, [U.UpdateVote])) where
    encode = encode . dmContents
    decode = do
        c@(up, votes) <- decode
        let !id = hash up
        -- FIXME don't do this in the decoder.
        unless (all ((id ==) . U.uvProposalId) votes) $ cborError $
            "decode@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

type InvOrData key contents = Either (InvMsg key) (DataMsg contents)

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
