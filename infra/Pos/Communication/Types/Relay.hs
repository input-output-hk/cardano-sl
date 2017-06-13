{-# LANGUAGE TypeFamilies #-}

module Pos.Communication.Types.Relay
       ( InvMsg (..)
       , ReqMsg (..)
       , MempoolMsg (..)
       , DataMsg (..)
       , InvOrData
       , InvOrDataTK
       ) where

import           Control.Lens        (Wrapped (..), iso)
import           Data.Tagged         (Tagged)
import qualified Data.Text.Buildable as B
import           Formatting          (bprint, build, (%))
import           Universum

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

data MempoolMsg tag = MempoolMsg
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
