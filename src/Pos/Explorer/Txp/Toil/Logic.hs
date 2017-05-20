{-# LANGUAGE TypeFamilies #-}

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
import qualified Data.HashSet                as HS
import           Data.List                   (delete)
import qualified Data.List.NonEmpty          as NE

import           Pos.Core                    (Address, HeaderHash, Timestamp)
import           Pos.Crypto                  (WithHash (..), hash)
import           Pos.Explorer.Core           (AddrHistory, TxExtra (..))
import           Pos.Explorer.Txp.Toil.Class (MonadTxExtra (..), MonadTxExtraRead (..))
import           Pos.Txp.Core                (Tx (..), TxAux (..), TxId, TxOut (..),
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
    applier (i, (txAux, txundo)) = do
        let tx = taTx txAux
            id = hash tx
            newExtra = TxExtra (Just (hh, i)) curTime txundo
        extra <- fromMaybe newExtra <$> getTxExtra id
        putTxExtraWithHistory id extra $ getTxRelatedAddrs txAux txundo

-- | Rollback transactions from one block.
eRollbackToil :: EGlobalToilMode m => [(TxAux, TxUndo)] -> m ()
eRollbackToil txun = do
    Txp.rollbackToil txun
    mapM_ extraRollback txun
  where
    extraRollback (txAux, txundo) =
        delTxExtraWithHistory (hash (taTx txAux)) $
        getTxRelatedAddrs txAux txundo

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
    wHash (i, (txAux, _)) = WithHash (taTx txAux) i
    normalize = runExceptT . uncurry eProcessTx . repair
    repair (i, (txAux, extra)) = ((i, txAux), extra)

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
    for_ addrs $ modifyAddrHistory $
        NewestFirst . (id :) . getNewestFirst

delTxExtraWithHistory
    :: MonadTxExtra m
    => TxId
    -> NonEmpty Address
    -> m ()
delTxExtraWithHistory id addrs = do
    delTxExtra id
    for_ addrs $ modifyAddrHistory $
        NewestFirst . delete id . getNewestFirst

getTxRelatedAddrs :: TxAux -> TxUndo -> NonEmpty Address
getTxRelatedAddrs TxAux {taTx = UnsafeTx {..}} undo =
    map txOutAddress _txOutputs `unionNE` map (txOutAddress . toaOut) undo
  where
    toSet = HS.fromList . toList
    -- Safe here, because union of non-empty sets can't be empty.
    unionNE lhs rhs = NE.fromList $ toList $ HS.union (toSet lhs) (toSet rhs)
