module Pos.Core.Txp.TxPayload
       ( TxPayload (..)
       , mkTxPayload
       , checkTxPayload
       , txpTxs
       , txpWitnesses
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError)

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Txp.Tx
import           Pos.Core.Txp.TxAux
import           Pos.Core.Txp.TxWitness

-- | Payload of Txp component which is part of main block. Constructor
-- is unsafe, because it lets one create invalid payload, for example
-- with different number of transactions and witnesses.
data TxPayload = UnsafeTxPayload
    { -- | Transactions are the main payload.
      _txpTxs       :: ![Tx]
    , -- | Witnesses for each transaction.
      -- The payload is invalid if this list is not of the same length
      -- as '_txpTxs'. See 'mkTxPayload'r
      _txpWitnesses :: ![TxWitness]
    } deriving (Show, Eq, Generic)

instance NFData TxPayload

makeLenses ''TxPayload

-- | Smart constructor of 'TxPayload' which ensures that invariants of 'TxPayload' hold.
--
-- Currently there is only one invariant:
-- • number of txs must be same as number of witnesses.
mkTxPayload :: [TxAux] -> TxPayload
mkTxPayload txws = do
    UnsafeTxPayload {..}
  where
    (txs, _txpWitnesses) =
            unzip . map (liftA2 (,) taTx taWitness) $ txws
    _txpTxs = txs

instance Bi TxPayload where
    encode UnsafeTxPayload {..} = encode $ zip (toList _txpTxs) _txpWitnesses
    decode = mkTxPayload <$> decode

-- | Check a TxPayload by checking all of the Txs it contains.
checkTxPayload :: MonadError Text m => TxPayload -> m ()
checkTxPayload it = forM_ (_txpTxs it) checkTx
