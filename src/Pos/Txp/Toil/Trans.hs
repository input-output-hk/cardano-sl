-- | ToilT monad transformer. Single-threaded.

module Pos.Txp.Toil.Trans
       ( ToilT (..)
       , runToilT
       , runToilTGlobal
       , runToilTLocal
       , execToilTLocal
       , runToilTLocalExtra
       ) where

import           Control.Lens              (at, to, (%=), (+=), (.=))
import           Control.Monad.Except      (MonadError)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Default              (Default (def))
import qualified Data.HashMap.Strict       as HM
import           System.Wlog               (CanLog, HasLoggerName)
import           Universum

import           Pos.Context               (WithNodeContext)
import           Pos.Slotting.Class        (MonadSlots)
import           Pos.Slotting.MemState     (MonadSlotsData)
import           Pos.Txp.Toil.Class        (MonadBalances (..), MonadBalancesRead (..),
                                            MonadToilEnv, MonadTxPool (..),
                                            MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Types        (GenericToilModifier (..), MemPool,
                                            ToilModifier, UndoMap, UtxoModifier, bvStakes,
                                            bvTotal, mpLocalTxs, mpLocalTxsSize,
                                            tmBalances, tmMemPool, tmUndos, tmUtxo)
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

newtype ToilT ext m a = ToilT
    { getToilT :: StateT (GenericToilModifier ext) m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadFail
               , MonadTrans
               , MonadIO
               , MonadError e
               , MonadFix
               , MonadThrow
               , MonadCatch
               , MonadMask

               , HasLoggerName
               , CanLog

               , WithNodeContext ssc
               , MonadJL
               , MonadSlotsData
               , MonadSlots
               , MonadToilEnv
               )

instance MonadUtxoRead m => MonadUtxoRead (ToilT __ m) where
    utxoGet id = ToilT $ MM.lookupM utxoGet id =<< use tmUtxo

instance MonadUtxoRead m =>
         MonadUtxo (ToilT __ m) where
    utxoPut id aux = ToilT $ tmUtxo %= MM.insert id aux
    utxoDel id = ToilT $ tmUtxo %= MM.delete id

instance MonadBalancesRead m =>
         MonadBalancesRead (ToilT __ m) where
    getStake id =
        ToilT $ (<|>) <$> use (tmBalances . bvStakes . at id) <*> getStake id
    getTotalStake =
        ToilT $ maybe getTotalStake pure =<< use (tmBalances . bvTotal)

instance MonadBalancesRead m => MonadBalances (ToilT __ m) where
    setStake id c = ToilT $ tmBalances . bvStakes . at id .= Just c

    setTotalStake c = ToilT $ tmBalances . bvTotal .= Just c

instance Monad m => MonadTxPool (ToilT __ m) where
    hasTx id = ToilT $ use $ tmMemPool . mpLocalTxs . to (HM.member id)

    putTxWithUndo id tx undo = ToilT $ do
        has <- use $ tmMemPool . mpLocalTxs . to (HM.member id)
        unless has $ do
            tmMemPool . mpLocalTxs . at id .= Just tx
            tmMemPool . mpLocalTxsSize += 1
            tmUndos . at id .= Just undo

    poolSize = ToilT $ use $ tmMemPool . mpLocalTxsSize

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

-- | Run ToilT using specified modifier.
runToilT :: GenericToilModifier ext
         -> ToilT ext m a
         -> m (a, GenericToilModifier ext)
runToilT txm (ToilT st) = runStateT st txm

-- | Run ToilT using empty modifier. Should be used for global
-- transaction processing.
runToilTGlobal
    :: (Default ext, Functor m)
    => ToilT ext m a -> m (a, GenericToilModifier ext)
runToilTGlobal txpt = runToilT def txpt

-- | Run ToilT using empty balances modifier. Should be used for local
-- transaction processing.
runToilTLocal
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> ToilT () m a
    -> m (a, ToilModifier)
runToilTLocal um mp undo txpt =
    runToilT (def {_tmUtxo = um, _tmMemPool = mp, _tmUndos = undo}) txpt

-- | Execute ToilT using empty balances modifier. Should be used for
-- local transaction processing.
execToilTLocal
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> ToilT () m a
    -> m (ToilModifier)
execToilTLocal um mp undo = fmap snd . runToilTLocal um mp undo

-- | Like 'runToilTLocal', but takes extra data as argument.
runToilTLocalExtra
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> extra
    -> ToilT extra m a
    -> m (a, GenericToilModifier extra)
runToilTLocalExtra um mp undo e =
    runToilT
        (ToilModifier
         { _tmUtxo = um
         , _tmBalances = def
         , _tmMemPool = mp
         , _tmUndos = undo
         , _tmExtra = e
         })
