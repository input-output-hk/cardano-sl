-- | Binary serialization of network Txp types.

module Pos.Binary.Txp.Network
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), label, labelS, putField)
import qualified Pos.Binary.Cbor               as Cbor
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Txp.Network.Types         (TxMsgContents (..))

----------------------------------------------------------------------------
-- Network
----------------------------------------------------------------------------

instance Bi (DataMsg TxMsgContents) where
    sizeNPut = labelS "DataMsg TxMsgContents" $
        putField (\(DataMsg (TxMsgContents txAux)) -> txAux)
    get = label "DataMsg TxMsgContents" $
        DataMsg <$> (TxMsgContents <$> get)

instance Cbor.Bi (DataMsg TxMsgContents) where
  encode (DataMsg (TxMsgContents txAux)) = Cbor.encode txAux
  decode = DataMsg <$> (TxMsgContents <$> Cbor.decode)
