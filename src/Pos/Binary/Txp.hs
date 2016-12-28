-- | Binary serialization for Pos.Txp.*

module Pos.Binary.Txp () where

import           Universum

import           Pos.Binary.Class            (Bi (..))
import           Pos.Txp.Types.Communication (TxDataMsg (..), TxInvMsg (..),
                                              TxReqMsg (..))

instance Bi TxInvMsg where
    put (TxInvMsg imTxs) = put imTxs
    get = TxInvMsg <$> get

instance Bi TxReqMsg where
    put (TxReqMsg reqTxs) = put reqTxs
    get = TxReqMsg <$> get

instance Bi TxDataMsg where
    put (TxDataMsg dmTx dmWitness dmDistr) =
        put dmTx >> put dmWitness >> put dmDistr
    get = TxDataMsg <$> get <*> get <*> get
