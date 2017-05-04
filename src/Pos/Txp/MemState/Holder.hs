{-# LANGUAGE TypeFamilies #-}

-- | Monad transformer which implements MonadTxpMem based on ReaderT.

module Pos.Txp.MemState.Holder
       ( TxpHolderTag
       , GenericTxpLocalData
       , mkTxpLocalData
       ) where

import qualified Control.Concurrent.STM as STM
import           Data.Default           (Default (def))
import           Universum

import           Pos.Types              (HeaderHash)

import           Pos.Txp.MemState.Class (TxpHolderTag)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..))
import           Pos.Txp.Toil.Types     (UtxoModifier)

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------

mkTxpLocalData
    :: (Default e, MonadIO m)
    => UtxoModifier -> HeaderHash -> m (GenericTxpLocalData e)
mkTxpLocalData uv initTip = TxpLocalData
    <$> liftIO (STM.newTVarIO uv)
    <*> liftIO (STM.newTVarIO def)
    <*> liftIO (STM.newTVarIO mempty)
    <*> liftIO (STM.newTVarIO initTip)
    <*> liftIO (STM.newTVarIO def)
