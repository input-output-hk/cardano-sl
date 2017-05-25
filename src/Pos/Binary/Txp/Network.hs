-- | Binary serialization of network Txp types.

module Pos.Binary.Txp.Network
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), label)
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Txp.Network.Types         (TxMsgContents (..))

----------------------------------------------------------------------------
-- Network
----------------------------------------------------------------------------

instance Bi (DataMsg TxMsgContents) where
    put (DataMsg (TxMsgContents txAux)) =
        put txAux
    get = label "DataMsg TxMsgContents" $
        DataMsg <$> (TxMsgContents <$> get)
