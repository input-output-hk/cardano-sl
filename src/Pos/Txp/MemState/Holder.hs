{-# LANGUAGE TypeFamilies #-}

-- | Monad transformer which implements MonadTxpMem based on ReaderT.

module Pos.Txp.MemState.Holder
       ( TxpHolder
       , mkTxpLocalData
       , runTxpHolder
       ) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Ether    as Ether.E
import           Data.Default           (Default (def))
import           Universum

import           Pos.Types              (HeaderHash)

import           Pos.Txp.MemState.Class (TxpHolderTag)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..), TxpMetrics (..))
import           Pos.Txp.Toil.Types     (UtxoModifier)

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------

type TxpHolder ext = Ether.E.ReaderT TxpHolderTag (GenericTxpLocalData ext, TxpMetrics)

mkTxpLocalData
    :: (Default e, MonadIO m)
    => UtxoModifier -> HeaderHash -> m (GenericTxpLocalData e)
mkTxpLocalData uv initTip = TxpLocalData
    <$> liftIO (STM.newTVarIO uv)
    <*> liftIO (STM.newTVarIO def)
    <*> liftIO (STM.newTVarIO mempty)
    <*> liftIO (STM.newTVarIO initTip)
    <*> liftIO (STM.newTVarIO def)

runTxpHolder :: GenericTxpLocalData ext -> TxpMetrics -> TxpHolder ext m a -> m a
runTxpHolder dat met = flip (Ether.E.runReaderT (Proxy @TxpHolderTag)) (dat, met)
