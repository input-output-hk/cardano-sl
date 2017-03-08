
-- | Functions operating on UTXO.

module Pos.Txp.Toil.Utxo.Functions
       ( verifyTxUtxo
       , applyTxToUtxo
       , rollbackTxUtxo
       -- * Pure
       , deleteTxIn
       , findTxIn
       , belongsTo
       , filterUtxoByAddr
       , utxoToStakes
       ) where

import           Control.Monad.Error.Class (MonadError (..))
import qualified Data.HashMap.Strict       as HM
import qualified Data.List.NonEmpty        as NE
import qualified Data.Map.Strict           as M
import qualified Data.Text                 as T
import           Universum

import           Pos.Binary.Core           ()
import           Pos.Core                  (Address, Coin, StakeholderId, unsafeAddCoin)
import           Pos.Crypto                (WithHash (..), hash)
import           Pos.Txp.Core              (Tx (..), TxAux, TxDistribution (..),
                                            TxIn (..), TxOut (..), TxOutAux, TxUndo,
                                            txOutStake)
import           Pos.Txp.Core.Tx           (VTxGlobalContext (..), VTxLocalContext (..),
                                            verifyTx)
import           Pos.Txp.Toil.Class        (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Failure      (TxpVerFailure (..))
import           Pos.Txp.Toil.Types        (Utxo)

-- CHECK: @verifyTxUtxo
-- | Verify single Tx using MonadUtxoRead as TxIn resolver.
verifyTxUtxo
    :: (MonadUtxoRead m, MonadError TxpVerFailure m)
    => Bool
    -> TxAux
    -> m TxUndo
verifyTxUtxo verifyVersions txaux = do
    res <- verifyTx verifyVersions VTxGlobalContext utxoGet' txaux
    case res of
        Left errors -> throwError $ TxpInvalid $ T.intercalate ";\n" errors
        Right undo  -> pure undo
  where
    utxoGet' x = fmap VTxLocalContext <$> utxoGet x

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: MonadUtxo m => WithHash Tx -> TxDistribution -> m ()
applyTxToUtxo (WithHash UnsafeTx {..} txid) distr = do
    mapM_ utxoDel _txInputs
    mapM_ applyOutput . zip [0 ..] . toList . NE.zip _txOutputs $
        getTxDistribution distr
  where
    applyOutput (idx, (out, ds)) = utxoPut (TxIn txid idx) (out, ds)

rollbackTxUtxo
    :: (MonadError TxpVerFailure m, MonadUtxo m)
    => (TxAux, TxUndo) -> m ()
rollbackTxUtxo ((tx@UnsafeTx{..}, _, _), undo) = do
    unless (length _txInputs == length undo) $
        throwError $ TxpInvalidUndoLength (length _txInputs) (length undo)
    let txid = hash tx
    mapM_ utxoDel $ take (length _txOutputs) $ zipWith TxIn (repeat txid) [0..]
    mapM_ (uncurry utxoPut) $ zip (toList _txInputs) undo

----------------------------------------------------------------------------
-- Pure
----------------------------------------------------------------------------

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOutAux
findTxIn = M.lookup

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn = M.delete

-- | A predicate for `TxOut` which selects outputs for given address
belongsTo :: TxOutAux -> Address -> Bool
(out, _) `belongsTo` addr = addr == txOutAddress out

-- | Select only TxOuts for given addresses
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`belongsTo` addr)

utxoToStakes :: Utxo -> HashMap StakeholderId Coin
utxoToStakes = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    putDistr hm (_, toaux) = foldl' plusAt hm (txOutStake toaux)
