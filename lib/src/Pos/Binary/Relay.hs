-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Binary.Crypto ()
import           Pos.Binary.Ssc ()
import           Pos.Binary.Update ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core (ProxySKHeavy)

instance Bi (DataMsg ProxySKHeavy) where
    encode = encode . dmContents
    decode = DataMsg <$> decode
