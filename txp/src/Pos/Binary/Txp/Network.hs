{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary serialization of network Txp types.

module Pos.Binary.Txp.Network
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Txp.Network.Types (TxMsgContents (..))

----------------------------------------------------------------------------
-- Network
----------------------------------------------------------------------------

instance Bi (DataMsg TxMsgContents) where
    encode (DataMsg (TxMsgContents txAux)) = encode txAux
    decode = DataMsg <$> (TxMsgContents <$> decode)
