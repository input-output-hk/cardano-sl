-- | ToilT monad transformer. Single-threaded.

module Pos.Txp.Toil.Trans
       ( ToilT (..)
       , runToilT
       , runToilTGlobal
       , runToilTLocal
       , execToilTLocal
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
import           Pos.Txp.Toil.Types        (BalancesView, MemPool, ToilModifier (..),
                                            UndoMap, UtxoModifier, bvStakes, bvTotal,
                                            mpLocalTxs, mpLocalTxsSize, tmBalances,
                                            tmMemPool, tmUndos, tmUtxo)
import           Pos.Util.JsonLog          (MonadJL (..))
import qualified Pos.Util.Modifier         as MM

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores ToilModifier and implements
-- writable Toil type classes.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
-- Used for block application now.

newtype ToilT m a = ToilT
    { getToilT :: StateT ToilModifier m a
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

instance MonadUtxoRead m => MonadUtxoRead (ToilT m) where
    utxoGet id = ToilT $ MM.lookupM utxoGet id =<< use tmUtxo

instance MonadUtxoRead m =>
         MonadUtxo (ToilT m) where
    utxoPut id aux = ToilT $ tmUtxo %= MM.insert id aux
    utxoDel id = ToilT $ tmUtxo %= MM.delete id

instance MonadBalancesRead m => MonadBalancesRead (ToilT m) where
    getStake id = ToilT $
        (<|>) <$> use (tmBalances . bvStakes . at id)
              <*> getStake id

    getTotalStake = ToilT $ use $ tmBalances . bvTotal

instance MonadBalancesRead m => MonadBalances (ToilT m) where
    setStake id c = ToilT $ tmBalances . bvStakes . at id .= Just c

    setTotalStake c = ToilT $ tmBalances . bvTotal .= c

instance Monad m => MonadTxPool (ToilT m) where
    hasTx id = ToilT $ use $ tmMemPool . mpLocalTxs . to (HM.member id)

    putTxWithUndo id tx undo = ToilT $ do
        has <- use $ tmMemPool . mpLocalTxs . to (HM.member id)
        unless has $ do
            tmMemPool . mpLocalTxs . at id .= Just tx
            tmMemPool . mpLocalTxsSize %= (+1)
            tmUndos . at id .= Just undo

    poolSize = ToilT $ use $ tmMemPool . mpLocalTxsSize

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runToilT :: ToilModifier -> ToilT m a -> m (a, ToilModifier)
runToilT txm (ToilT st) = runStateT st txm

runToilTGlobal :: Functor m => BalancesView -> ToilT m a -> m (a, ToilModifier)
runToilTGlobal bv txpt = runToilT (ToilModifier mempty bv def mempty) txpt

runToilTLocal
    :: Functor m
    => UtxoModifier -> MemPool -> UndoMap -> ToilT m a -> m (a, ToilModifier)
runToilTLocal um mp undo txpt = runToilT (ToilModifier um def mp undo) txpt

execToilTLocal
    :: Functor m
    => UtxoModifier -> MemPool -> UndoMap -> ToilT m a -> m ToilModifier
execToilTLocal um mp undo txpt = snd <$> runToilT (ToilModifier um def mp undo) txpt
