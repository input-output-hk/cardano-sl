{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Types used for stats requests

module Pos.Communication.Types.Statistics
       ( RequestStat (..)
       , ResponseStat (..)
       ) where

import           Data.Binary              (Binary)
import           Data.MessagePack         (MessagePack)
import           Universum

import           Control.TimeWarp.Rpc     (Message (..))

import           Pos.Statistics.StatEntry (StatLabel (..))

{-
`RequestStat` and `Response` need to carry message ids
so stats collector can match answers with queries.
TODO: remove after `ListenerDHT` is able to know sender's address
-}

-- | Message: someone requested a stat
data RequestStat l =
    RequestStat !Word64 !l
    deriving (Generic)

-- | Message: send the list with stats back
data ResponseStat l a =
    ResponseStat !Word64 !l !(Maybe [a])
    deriving (Generic)

instance StatLabel l => Binary (RequestStat l)
instance (StatLabel l, Binary a) => Binary (ResponseStat l a)

instance StatLabel l => MessagePack (RequestStat l)
instance (StatLabel l, MessagePack a) => MessagePack (ResponseStat l a)

instance StatLabel l => Message (RequestStat l) where
    messageName _ = "RequestStat_" <> labelName (Proxy :: Proxy l)

instance (StatLabel l, Typeable a) => Message (ResponseStat l a) where
    messageName _ = "ResponseStat_" <> labelName (Proxy :: Proxy l)
