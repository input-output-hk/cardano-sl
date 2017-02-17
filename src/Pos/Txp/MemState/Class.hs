{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Txp.MemState.Class
       ( MonadTxpMem (..)
       -- , getTxpLD
       -- , modifyTxpLD
       -- , modifyTxpLD_

       -- , getLocalUndo
       , getUtxoView
       , getLocalTxsNUndo
       , getMemPool
       , getLocalTxs
       ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except   (ExceptT)
import           Control.Monad.State    (StateT)
import           Control.Monad.Trans    (MonadTrans)
import qualified Data.HashMap.Strict    as HM
import           Universum

import           Pos.DHT.Real           (KademliaDHT)
import           Pos.Txp.MemState.Types (TxpLocalData)
import           Pos.Txp.Txp.Types      (MemPool (localTxs), UtxoView)
import           Pos.Types              (HeaderHash, TxAux, TxId, TxOutAux)



-- | Reduced equivalent of @MonadReader TxpLDWrap m@.
class Monad m => MonadTxpMem m where
    askTxpMem :: m TxpLocalData
    -- ^ Retrieve 'TxpLDWrap'.

    -- | Default implementation for 'MonadTrans'.
    default askTxpMem
        :: (MonadTrans t, MonadTxpMem m', t m' ~ m) => m TxpLocalData
    askTxpMem = lift askTxpMem

instance MonadTxpMem m => MonadTxpMem (ReaderT s m)
instance MonadTxpMem m => MonadTxpMem (StateT s m)
instance MonadTxpMem m => MonadTxpMem (ExceptT s m)
instance MonadTxpMem m => MonadTxpMem (KademliaDHT m)

-- getTxpLD :: (MonadIO m, MonadTxpMem m) => m Tx
-- getTxpLD = askTxpMem >>= \txld -> atomically $
--     (,,,) <$> STM.readTVar (utxoView txld)
--           <*> STM.readTVar (memPool txld)
--           <*> STM.readTVar (undos txld)
--           <*> STM.readTVar (ldTip txld)

-- modifyTxpLD :: (MonadIO m, MonadTxpMem m) => (TxpLD ssc -> (a, TxpLD ssc)) -> m a
-- modifyTxpLD f =
--     askTxpMem >>= \txld -> atomically $ do
--         curUV  <- STM.readTVar (utxoView txld)
--         curMP  <- STM.readTVar (memPool txld)
--         curUndos <- STM.readTVar (undos txld)
--         curTip <- STM.readTVar (ldTip txld)
--         let (res, (newUV, newMP, newUndos, newTip))
--               = f (curUV, curMP, curUndos, curTip)
--         STM.writeTVar (utxoView txld) newUV
--         STM.writeTVar (memPool txld) newMP
--         STM.writeTVar (undos txld) newUndos
--         STM.writeTVar (ldTip txld) newTip

-- modifyTxpLD_ :: (MonadIO m, MonadTxpMem m) => (TxpLD ssc -> TxpLD ssc) -> m ()
-- modifyTxpLD_ = modifyTxpLD . (((),) .)


-- getLocalUndo :: MonadTxpLD ssc m => m (HashMap TxId [TxOutAux])
-- getLocalUndo = (\(_, _, undos, _) -> undos) <$> getTxpLD

getUtxoView :: MonadTxpMem m => m UtxoView
getUtxoView = notImplemented
-- getUtxoView = (\(uv, _, _, _) -> uv) <$> getTxpLD

getLocalTxs :: MonadTxpMem m => m [(TxId, TxAux)]
getLocalTxs = HM.toList . localTxs <$> getMemPool

getLocalTxsNUndo :: MonadTxpMem m => m ([(TxId, TxAux)], HashMap TxId [TxOutAux])
getLocalTxsNUndo = notImplemented
-- getLocalTxsNUndo = (\(_, mp, undos, _) -> (HM.toList . localTxs $ mp, undos))
--                 <$> getTxpLD

getMemPool :: MonadTxpMem m => m MemPool
getMemPool = notImplemented
-- getMemPool = (\(_, mp, _, _) -> mp) <$> getTxpLD
