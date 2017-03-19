{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Explorer's version of Toil logic.

module Pos.Explorer.Txp.Toil.Logic
       ( EGlobalToilMode
       , eApplyToil
       , eRollbackToil
       , eNormalizeToil
       , eProcessTx
       ) where

import           Universum

import           Control.Monad.Except        (MonadError (..))

import           Pos.Crypto                  (WithHash (..), hash)

import           Data.List                   (delete, union)
import qualified Data.List.NonEmpty          as NE
import           Pos.Core                    (Address, HeaderHash, Timestamp)
import           Pos.Explorer.Core           (AddrHistory, TxExtra (..))
import           Pos.Explorer.Txp.Toil.Class (MonadTxExtra (..), MonadTxExtraRead (..))
import           Pos.Txp.Core                (Tx (..), TxAux, TxId, TxOut (..),
                                              TxOutAux (..), TxUndo, topsortTxs)
import           Pos.Txp.Toil                (ToilVerFailure (..))
import qualified Pos.Txp.Toil                as Txp
import           Pos.Util.Chrono             (NewestFirst (..))

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type EGlobalToilMode m = ( Txp.GlobalToilMode m
                         , MonadTxExtra m
                         )

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
eApplyToil
    :: EGlobalToilMode m
    => Timestamp
    -> [(TxAux, TxUndo)]
    -> HeaderHash
    -> m ()
eApplyToil curTime txun hh = do
    Txp.applyToil txun
    mapM_ applier $ zip [0..] txun
  where
    applier (i, (txaux@(tx, _, _), txundo)) = do
        let id = hash tx
            newExtra = TxExtra (Just (hh, i)) curTime txundo
        extra <- maybe newExtra identity <$> getTxExtra id
        putTxExtraWithHistory id extra $ getTxRelatedAddrs txaux txundo

-- | Rollback transactions from one block.
eRollbackToil :: EGlobalToilMode m => [(TxAux, TxUndo)] -> m ()
eRollbackToil txun = do
    Txp.rollbackToil txun
    mapM_ extraRollback txun
  where
    extraRollback (txaux@(tx, _, _), txundo) =
        delTxExtraWithHistory (hash tx) $ getTxRelatedAddrs txaux txundo

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type ELocalToilMode m = ( Txp.LocalToilMode m
                        , MonadTxExtra m
                        )

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
eProcessTx
    :: (ELocalToilMode m, MonadError ToilVerFailure m)
    => (TxId, TxAux) -> TxExtra -> m ()
eProcessTx tx@(id, aux) extra = do
    undo <- Txp.processTx tx
    putTxExtraWithHistory id extra $ getTxRelatedAddrs aux undo

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
eNormalizeToil
    :: (ELocalToilMode m)
    => [(TxId, (TxAux, TxExtra))]
    -> m ()
eNormalizeToil txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, ((t, _, _), _)) = WithHash t i
    normalize = runExceptT . uncurry eProcessTx . repair
    repair (i, (txaux, extra)) = ((i, txaux), extra)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

modifyAddrHistory
    :: MonadTxExtra m
    => (AddrHistory -> AddrHistory)
    -> Address
    -> m ()
modifyAddrHistory f addr =
    updateAddrHistory addr . f =<< getAddrHistory addr

putTxExtraWithHistory
    :: MonadTxExtra m
    => TxId
    -> TxExtra
    -> NonEmpty Address
    -> m ()
putTxExtraWithHistory id extra addrs = do
    putTxExtra id extra
    forM_ addrs $ modifyAddrHistory $
        NewestFirst . (id :) . getNewestFirst

delTxExtraWithHistory
    :: MonadTxExtra m
    => TxId
    -> NonEmpty Address
    -> m ()
delTxExtraWithHistory id addrs = do
    delTxExtra id
    forM_ addrs $ modifyAddrHistory $
        NewestFirst . delete id . getNewestFirst

getTxRelatedAddrs :: TxAux -> TxUndo -> NonEmpty Address
getTxRelatedAddrs (UnsafeTx {..}, _, _) undo = NE.fromList $
    map txOutAddress (NE.toList _txOutputs) `union`
    map (txOutAddress . toaOut) (NE.toList undo)
