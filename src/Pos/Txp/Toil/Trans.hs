{-# LANGUAGE CPP #-}

-- | TxpT monad transformer. Single-threaded.

module Pos.Txp.Toil.Trans
       ( TxpT (..)
       , runTxpT
       , runTxpTGlobal
       , runTxpTLocal
       , execTxpTLocal
       ) where

import           Control.Lens              (at, to, (%=), (.=))
import           Control.Monad.Except      (MonadError)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Default              (def)
import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import           System.Wlog               (CanLog, HasLoggerName)
import           Universum

import           Pos.Context               (WithNodeContext)
import           Pos.Slotting.Class        (MonadSlots, MonadSlotsData)
import           Pos.Util.JsonLog          (MonadJL (..))


import           Pos.Txp.Toil.Types        (BalancesView, MemPool, TxpModifier (..),
                                            UndoMap, UtxoView, bvStakes, bvTotal,
                                            mpLocalTxs, mpLocalTxsSize, txmBalances,
                                            txmMemPool, txmUndos, txmUtxoView, uvAddUtxo,
                                            uvDelUtxo)
#ifdef DWITH_EXPLORER
import           Pos.Txp.Toil.Types        (mpLocalTxsExtra)
#endif
import           Pos.Txp.Toil.Class        (MonadBalances (..), MonadBalancesRead (..),
                                            MonadTxPool (..), MonadUtxo (..),
                                            MonadUtxoRead (..))

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores TxpModifier and implements
-- writable MonadTxp.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
-- Used for block application now.

newtype TxpT m a = TxpT
    { getTxpT :: StateT TxpModifier m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadThrow
               , MonadSlotsData
               , MonadSlots
               , MonadCatch
               , MonadIO
               , MonadFail
               , HasLoggerName
               , WithNodeContext ssc
               , MonadJL
               , CanLog
               , MonadMask
               , MonadError e
               , MonadFix)

instance MonadUtxoRead m => MonadUtxoRead (TxpT m) where
    utxoGet id = TxpT $ do
        deleted <- use $ txmUtxoView . uvDelUtxo . to (HS.member id)
        if | deleted -> pure Nothing
           | otherwise ->
               (<|>) <$> use (txmUtxoView . uvAddUtxo . at id)
                     <*> utxoGet id

instance MonadUtxoRead m => MonadUtxo (TxpT m) where
    utxoPut id aux = TxpT $ do
        txmUtxoView . uvDelUtxo . at id .= Nothing
        txmUtxoView . uvAddUtxo . at id .= Just aux

    utxoDel id = TxpT $ do
        inserted <- use $ txmUtxoView . uvAddUtxo . to (HM.member id)
        if | inserted  -> txmUtxoView . uvAddUtxo . at id .= Nothing
           | otherwise -> txmUtxoView . uvDelUtxo . at id .= Just ()

instance MonadBalancesRead m => MonadBalancesRead (TxpT m) where
    getStake id = TxpT $
        (<|>) <$> use (txmBalances . bvStakes . at id)
              <*> getStake id

    getTotalStake = TxpT $ use $ txmBalances . bvTotal

instance MonadBalancesRead m => MonadBalances (TxpT m) where
    setStake id c = TxpT $ txmBalances . bvStakes . at id .= Just c

    setTotalStake c = TxpT $ txmBalances . bvTotal .= c

instance Monad m => MonadTxPool (TxpT m) where
    hasTx id = TxpT $ use $ txmMemPool . mpLocalTxs . to (HM.member id)

    putTxWithUndo id tx undo = TxpT $ do
        has <- use $ txmMemPool . mpLocalTxs . to (HM.member id)
        unless has $ do
            txmMemPool . mpLocalTxs . at id .= Just tx
            txmMemPool . mpLocalTxsSize %= (+1)
            txmUndos . at id .= Just undo

    poolSize = TxpT $ use $ txmMemPool . mpLocalTxsSize

#ifdef DWITH_EXPLORER
    putTxExtra id txExtra = TxpT $
        txmMemPool . mpLocalTxsExtra . at id .= Just txExtra
#endif

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runTxpT :: TxpModifier -> TxpT m a -> m (a, TxpModifier)
runTxpT txm (TxpT st) = runStateT st txm

runTxpTGlobal :: Functor m => BalancesView -> TxpT m a -> m (a, TxpModifier)
runTxpTGlobal bv txpt = runTxpT (TxpModifier def bv def mempty) txpt

runTxpTLocal :: Functor m => UtxoView -> MemPool -> UndoMap -> TxpT m a -> m (a, TxpModifier)
runTxpTLocal uv mp undo txpt = runTxpT (TxpModifier uv def mp undo) txpt

execTxpTLocal :: Functor m => UtxoView -> MemPool -> UndoMap -> TxpT m a -> m TxpModifier
execTxpTLocal uv mp undo txpt = snd <$> runTxpT (TxpModifier uv def mp undo) txpt
