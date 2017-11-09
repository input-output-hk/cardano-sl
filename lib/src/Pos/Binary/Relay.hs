-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class              (Bi (..))
import           Pos.Binary.Crypto             ()
import           Pos.Binary.Ssc                ()
import           Pos.Binary.Update             ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Delegation.Types          (ProxySKLightConfirmation)
import           Pos.Types                     (ProxySKHeavy, ProxySKLight)

instance Bi (DataMsg ProxySKLight) where
    encode = encode . dmContents
    decode = DataMsg <$> decode

instance Bi (DataMsg ProxySKHeavy) where
    encode = encode . dmContents
    decode = DataMsg <$> decode

instance Bi (DataMsg ProxySKLightConfirmation) where
    encode = encode . dmContents
    decode = DataMsg <$> decode
