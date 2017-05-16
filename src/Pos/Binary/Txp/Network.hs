-- | Binary serialization of network Txp types.

module Pos.Binary.Txp.Network
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), label)
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Txp.Network.Types         (TxMsgTag (..))
import           Pos.Txp.Network.Types         (TxMsgContents (..))

----------------------------------------------------------------------------
-- Network
----------------------------------------------------------------------------

instance Bi TxMsgTag where
    put TxMsgTag = pure ()
    get = pure TxMsgTag

instance Bi (DataMsg TxMsgContents) where
    put (DataMsg (TxMsgContents dmTx dmWitness dmDistr)) =
        put dmTx >> put dmWitness >> put dmDistr
    get = label "DataMsg TxMsgContents" $
        DataMsg <$> (TxMsgContents <$> get <*> get <*> get)
