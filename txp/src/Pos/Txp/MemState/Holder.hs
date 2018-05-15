{-# LANGUAGE TypeFamilies #-}

-- | Monad transformer which implements MonadTxpMem based on ReaderT.

module Pos.Txp.MemState.Holder
       ( GenericTxpLocalData
       , mkTxpLocalData
       ) where

import           Universum

import           Data.Default (Default (def))

import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Common (getTip)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..))

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------

mkTxpLocalData
    :: (Default e, MonadIO m, MonadDBRead m)
    => m (GenericTxpLocalData e)
mkTxpLocalData = do
    initTip <- getTip
    TxpLocalData <$> newTVarIO mempty <*> newTVarIO def <*> newTVarIO mempty <*>
        newTVarIO initTip <*>
        newTVarIO def
