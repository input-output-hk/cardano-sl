-- | Delegation-related message types

module Pos.Communication.Types.Delegation
       ( SendProxySK (..)
       , ConfirmProxySK (..)
       , CheckProxySKConfirmed (..)
       , CheckProxySKConfirmedRes (..)
       ) where

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Universum

import           Pos.Types            (ProxySKEpoch, ProxySigEpoch)

-- | Message with delegated proxy secret key.
data SendProxySK =
    SendProxySK !ProxySKEpoch
    deriving (Show, Eq, Generic)

instance Message SendProxySK where
    messageName _ = "SendProxySK"
    formatMessage = messageName'

-- | Confirmation of proxy signature delivery. Delegate should take
-- the proxy signing key he has and sign this key with itself. If the
-- signature is correct, then it was done by delegate (guaranteed by
-- PSK scheme). Checking @w@ can be done with @(const True)@
-- predicate, because certificate may be sent in epoch id that's
-- before lower cert's @EpochIndex@.
data ConfirmProxySK =
    ConfirmProxySK !ProxySKEpoch !(ProxySigEpoch ProxySKEpoch)
    deriving (Show, Eq, Generic)

instance Message ConfirmProxySK where
    messageName _ = "ConfirmProxySK"
    formatMessage = messageName'

-- | Request to check if a node has any info about PSK delivery.
data CheckProxySKConfirmed =
    CheckProxySKConfirmed !ProxySKEpoch
    deriving (Show, Eq, Generic)

instance Message CheckProxySKConfirmed where
    messageName _ = "CheckProxySKConfirmed"
    formatMessage = messageName'

-- | Response to the @CheckProxySKConfirmed@ call.
data CheckProxySKConfirmedRes =
    CheckProxySKConfirmedRes !Bool
    deriving (Show, Eq, Generic)

instance Message CheckProxySKConfirmedRes where
    messageName _ = "CheckProxySKConfirmedRes"
    formatMessage = messageName'
