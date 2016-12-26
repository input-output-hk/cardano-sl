-- | Communication-related serialization

module Pos.Binary.Communication () where

import           Universum

import           Pos.Binary.Class        (Bi (..))
import           Pos.Communication.Types (CheckProxySKConfirmed (..),
                                          CheckProxySKConfirmedRes (..),
                                          ConfirmProxySK (..), SendProxySK (..),
                                          SysStartRequest (..), SysStartResponse (..))
import           Pos.Types               ()

instance Bi SysStartRequest where
    put = mempty
    get = pure SysStartRequest

instance Bi SysStartResponse where
    put (SysStartResponse t msid) = put t >> put msid
    get = SysStartResponse <$> get <*> get

instance Bi SendProxySK where
    put (SendProxySK pSk) = put pSk
    get = SendProxySK <$> get

instance Bi ConfirmProxySK where
    put (ConfirmProxySK pSk proof) = put pSk >> put proof
    get = liftA2 ConfirmProxySK get get

instance Bi CheckProxySKConfirmed where
    put (CheckProxySKConfirmed pSk) = put pSk
    get = CheckProxySKConfirmed <$> get

instance Bi CheckProxySKConfirmedRes where
    put (CheckProxySKConfirmedRes res) = put res
    get = CheckProxySKConfirmedRes <$> get
