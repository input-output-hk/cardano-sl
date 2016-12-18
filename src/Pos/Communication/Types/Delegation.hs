-- | Delegation-related message types

module Pos.Communication.Types.Delegation
       ( SendProxySK(..)
       , ConfirmProxySK(..)
       ) where

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Universum

import           Pos.Crypto           (ProxySecretKey, ProxySignature)
import           Pos.Types            (EpochIndex)

type PSK = ProxySecretKey (EpochIndex, EpochIndex)

-- | Message with delegated proxy secret key.
data SendProxySK = SendProxySK !PSK
    deriving (Generic)

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
    ConfirmProxySK !PSK !(ProxySignature (EpochIndex, EpochIndex) PSK)

instance Message ConfirmProxySK where
    messageName _ = "ConfirmProxySK"
    formatMessage = messageName'
