-- | Impure functions which are used by both local and global txp.

module Pos.Txp.Logic.Common
       ( buildUtxo
       , buildUtxoForRollback
       ) where

import           Universum

import qualified Data.Map as M (fromList)

import           Pos.Core.Txp (Tx (..), TxAux (..), TxIn (..), TxOutAux)
import           Pos.Crypto (hash)
import           Pos.DB.Class (MonadDBRead)
import qualified Pos.Txp.DB as DB
import           Pos.Txp.Toil (Utxo, UtxoModifier)
import qualified Pos.Util.Modifier as MM

-- | Build base 'Utxo' for given transactions considering given
-- 'UtxoModifier' (can be 'mempty'). 'Utxo' is built by resolving
-- inputs of all transactions to corresponding 'TxOutAux'es using data
-- from the DB.
buildUtxo ::
       forall m. (MonadDBRead m)
    => UtxoModifier
    -> [TxAux]
    -> m Utxo
buildUtxo = buildUtxoGeneric (toList . _txInputs)

-- | Build base 'Utxo' to rollback given transactions considering
-- given 'UtxoModifier'. 'Utxo' is built by resolving inputs build
-- from outputs of transactions to corresponding 'TxOutAux'es using
-- data from the DB.
--
-- Note: it's may seem unnecessary to do lookup in DB, because we know
-- transaction outputs, but we need to know whether the input is in
-- DB. It can be slightly optimized by not deserialiaing data from DB,
-- but it's minor, because rollbacks are rare.
--
-- Note2: we don't really need to build base 'Utxo' for rollback, we
-- can just delete and add some entries to utxo without checking
-- anything, but we have a sanity check in 'utxoDel' just in case. We
-- can consider dropping it, but it shouldn't be a problem (e. g. it
-- shouldn't be essential for performance, because rollbacks are
-- rare).
buildUtxoForRollback ::
       forall m. (MonadDBRead m)
    => UtxoModifier
    -> [TxAux]
    -> m Utxo
buildUtxoForRollback = buildUtxoGeneric toTxIns
  where
    toTxIns :: Tx -> [TxIn]
    toTxIns tx =
        let !txId = hash tx
        in map (\i -> TxInUtxo txId i)
           [0 .. fromIntegral (length (_txOutputs tx)) - 1]

buildUtxoGeneric ::
       forall m. (MonadDBRead m)
    => (Tx -> [TxIn])
    -> UtxoModifier
    -> [TxAux]
    -> m Utxo
buildUtxoGeneric toInputs utxoModifier txs = concatMapM buildForOne txs
  where
    buildForOne :: TxAux -> m Utxo
    buildForOne txAux = do
        let tx = taTx txAux
        let utxoLookupM :: TxIn -> m (Maybe (TxIn, TxOutAux))
            utxoLookupM txIn =
                fmap (txIn, ) <$> MM.lookupM DB.getTxOut txIn utxoModifier
        resolvedPairs <- mapM utxoLookupM (toInputs tx)
        return $ M.fromList $ catMaybes $ toList resolvedPairs
