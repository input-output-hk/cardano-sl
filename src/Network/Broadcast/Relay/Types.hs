{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Broadcast.Relay.Types
       ( RelayError (..)
       , PropagationMsg (..)
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , InvOrData
       , InvOrDataTK
       , propagationMsgProvenance
       ) where

import           Control.Lens                  (Wrapped (..), iso)
import qualified Data.Text.Buildable           as Buildable
import           Data.Tagged                   (Tagged)
import           Formatting                    (bprint, shown, build, (%))
import           Universum

import           Node                          (NodeId)
import qualified Node.Message.Class            as Msg

data RelayError = UnexpectedInv
                | UnexpectedData
  deriving (Generic, Show)

instance Exception RelayError

-- | A message to be propagated (relayed) to peers.
data PropagationMsg packingType where
    InvReqDataPM ::
        ( Msg.Message (InvOrData key contents)
        , Msg.Serializable packingType (InvOrData key contents)
        , Buildable key
        , Eq key
        , Msg.Message (ReqMsg key)
        , Msg.Serializable packingType (ReqMsg key))
        => !(Maybe NodeId) -- ^ The peer which sent it to us.
        -> !key            -- ^ The key (will 'InvMsg' this to peers).
        -> !contents       -- ^ The data associated with the key.
        -> PropagationMsg packingType
    DataOnlyPM ::
        ( Msg.Message (DataMsg contents)
        , Msg.Serializable packingType (DataMsg contents)
        , Buildable contents)
        => !(Maybe NodeId) -- ^ The peer which sent it to us.
        -> !contents       -- ^ The data.
        -> PropagationMsg packingType

instance Buildable (PropagationMsg packingType) where
    build (InvReqDataPM peer key _) =
        bprint ("<data from peer "%shown%" for key "%build%">") peer key
    build (DataOnlyPM peer conts) =
        bprint ("<data from peer "%shown%" "%build) peer (Buildable.build conts)

-- | The peer from which the data to be propagated has come (or Nothing if
--   it originated locally).
propagationMsgProvenance
    :: PropagationMsg packingType
    -> Maybe NodeId
propagationMsgProvenance (InvReqDataPM mPeer _ _) = mPeer
propagationMsgProvenance (DataOnlyPM mPeer _)     = mPeer

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg key = InvMsg
    { imKey :: !key
    }
    deriving (Show, Eq)

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg key = ReqMsg
    { rmKey :: !key
    }
    deriving (Show, Eq)

-- | Data message. Can be used to send actual data.
data DataMsg contents = DataMsg
    { dmContents :: !contents
    } deriving (Show, Eq)

type InvOrData key contents = Either (InvMsg key) (DataMsg contents)

-- | InvOrData with key tagged by contents
type InvOrDataTK key contents = InvOrData (Tagged contents key) contents

instance (Buildable contents) =>
         Buildable (DataMsg contents) where
    build (DataMsg contents) = bprint ("Data {" %build % "}") contents

instance Wrapped (DataMsg contents) where
    type Unwrapped (DataMsg contents) = contents
    _Wrapped' = iso dmContents DataMsg
