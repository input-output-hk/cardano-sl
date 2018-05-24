{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Communication.Types.Relay
       ( InvMsg (..)
       , ReqMsg (..)
       , ResMsg (..)
       , ReqOrRes
       , MempoolMsg (..)
       , DataMsg (..)
       , InvOrData
       , InvOrDataTK
       , RelayLogEvent (..)
       ) where

import           Control.Lens (Wrapped (..), iso)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Tagged (Tagged)
import qualified Data.Text.Buildable as B
import           Formatting (bprint, build, (%))
import           Universum

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg key = InvMsg
    { imKey :: !key
    -- ^ Key for the data that you wish to announce.
    }
    deriving (Show, Eq)

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg key = ReqMsg
    { rmKey :: !(Maybe key)
    -- ^ Optional key for the data that you request.
    }
    deriving (Show, Eq)

data MempoolMsg tag = MempoolMsg
    deriving (Show, Eq)

-- | Data message. Can be used to send actual data.
data DataMsg contents = DataMsg
    { dmContents :: !contents
    } deriving (Generic, Show, Eq)

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

$(deriveJSON defaultOptions ''RelayLogEvent)
