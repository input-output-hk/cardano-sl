module Pos.Txp.Txp.Trans
    ( TxpT (..)
    ) where

import           Control.Monad.Except      (MonadError)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Trans.Class (MonadTrans)
import           System.Wlog               (CanLog, HasLoggerName, logWarning)
import           Universum

import           Pos.Context               (WithNodeContext)
import           Pos.Slotting.Class        (MonadSlots, MonadSlotsData)
import           Pos.Util.JsonLog          (MonadJL (..))

import           Pos.Txp.Txp.Class         (MonadTxpRead (..))
import           Pos.Txp.Txp.Types         (TxpModifier (..))


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

instance MonadTxpRead m => MonadTxpRead (TxpT m) where
    utxoGet = notImplemented
    getStake = notImplemented
    getTotalStake = notImplemented
    hasTx = notImplemented
