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
import           System.Wlog               (CanLog, HasLoggerName)
import           Universum

import           Pos.Context               (WithNodeContext)
import           Pos.Slotting.Class        (MonadSlots)
import           Pos.Slotting.MemState     (MonadSlotsData)
import           Pos.Txp.Toil.Class        (MonadBalances (..), MonadBalancesRead (..),
                                            MonadTxPool (..), MonadUtxo (..),
                                            MonadUtxoRead (..))
import           Pos.Txp.Toil.Types        (BalancesView, MemPool, TxpModifier (..),
                                            UndoMap, UtxoModifier, bvStakes, bvTotal,
                                            mpLocalTxs, mpLocalTxsSize, txmBalances,
                                            txmMemPool, txmUndos, txmUtxoModifier)
import           Pos.Util.JsonLog          (MonadJL (..))
import qualified Pos.Util.Modifier         as MM

#ifdef WITH_EXPLORER
import           Pos.Txp.Toil.Class        (MonadTxExtra (..), MonadTxExtraRead (..))
import           Pos.Txp.Toil.Types        (mpLocalTxsExtra)
#endif

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
    utxoGet id = TxpT $ MM.lookupM utxoGet id =<< use txmUtxoModifier

instance MonadUtxoRead m =>
         MonadUtxo (TxpT m) where
    utxoPut id aux = TxpT $ txmUtxoModifier %= MM.insert id aux
    utxoDel id = TxpT $ txmUtxoModifier %= MM.delete id

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

#ifdef WITH_EXPLORER
instance MonadTxExtraRead m => MonadTxExtraRead (TxpT m) where
    getTxExtra id = TxpT $
        (<|>) <$> use (txmMemPool . mpLocalTxsExtra . at id)
              <*> getTxExtra id

instance MonadTxExtraRead m => MonadTxExtra (TxpT m) where
    putTxExtra id extra = TxpT $
        txmMemPool . mpLocalTxsExtra . at id .= Just extra
#endif

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runTxpT :: TxpModifier -> TxpT m a -> m (a, TxpModifier)
runTxpT txm (TxpT st) = runStateT st txm

runTxpTGlobal :: Functor m => BalancesView -> TxpT m a -> m (a, TxpModifier)
runTxpTGlobal bv txpt = runTxpT (TxpModifier mempty bv def mempty) txpt

runTxpTLocal
    :: Functor m
    => UtxoModifier -> MemPool -> UndoMap -> TxpT m a -> m (a, TxpModifier)
runTxpTLocal um mp undo txpt = runTxpT (TxpModifier um def mp undo) txpt

execTxpTLocal
    :: Functor m
    => UtxoModifier -> MemPool -> UndoMap -> TxpT m a -> m TxpModifier
execTxpTLocal um mp undo txpt = snd <$> runTxpT (TxpModifier um def mp undo) txpt
