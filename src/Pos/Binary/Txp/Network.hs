-- | Binary serialization of network Txp types.

module Pos.Binary.Txp.Network
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), label, putField)
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Txp.Network.Types         (TxMsgContents (..))

----------------------------------------------------------------------------
-- Network
----------------------------------------------------------------------------

instance Bi (DataMsg TxMsgContents) where
    sizeNPut = putField (\(DataMsg (TxMsgContents txAux)) -> txAux)
    get = label "DataMsg TxMsgContents" $
        DataMsg <$> (TxMsgContents <$> get)
