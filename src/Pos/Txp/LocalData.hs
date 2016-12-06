{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Pos.Txp.LocalData
       (
         TxLocalData (..)
       , MonadTxLD (..)
       , txRunQuery
       , txRunUpdate

       , getLocalTxs
       , removeLocalTx
       , txApplyGlobalUtxo
       , txLocalDataRollback
       , txLocalDataProcessTx
       ) where

import           Control.Lens            (makeClassy, use, uses, view, (%=), (+=), (-=),
                                          (.=))
import qualified Data.Cache.LRU          as LRU
import           Data.Default            (Default (..))
import qualified Data.HashSet            as HS
import           Serokell.Util           (VerificationRes (..))
import           Universum

import           Pos.Constants           (maxLocalTxs)
import           Pos.Crypto              (WithHash (..))
import           Pos.State.Storage.Types (ProcessTxRes (..), mkPTRinvalid)
import           Pos.Types               (Tx (..), TxId, Utxo, normalizeTxs, verifyTxUtxo)
import           Pos.Util                (clearLRU)

data TxLocalData = TxLocalData
    { -- | Local set of transactions. These are valid (with respect to
      -- the head of last block's utxo) transactions which are known
      -- to the node and are /not/ included in the blockchain store by
      -- the node. This set is used later to form the block payload.
      _txLocalTxs     :: !(HashSet (WithHash Tx))
    , -- | @length@ is @O(n)@ for 'HE.HashSet' so we store it explicitly.
      _txLocalTxsSize :: !Int
    , -- | Transactions recently added to blocks. Used for ignoring
      -- overhead when same transaction is propagated several times.
      _txFilterCache  :: !(LRU.LRU TxId ())
    }

instance Default TxLocalData where
    def = TxLocalData
        { _txLocalTxs = mempty
        , _txLocalTxsSize = 0
        , _txFilterCache = LRU.newLRU (Just 10000)
        }

class Monad m => MonadTxLD m where
    getTxLocalData :: m TxLocalData
    setTxLocalData :: TxLocalData -> m ()

makeClassy ''TxLocalData
type Query a = forall m x. (HasTxLocalData x, MonadReader x m) => m a
type Update a = forall m x. (HasTxLocalData x, MonadState x m) => m a

-- | Convenient wrapper to run Query in MonadTxLD
txRunQuery :: MonadTxLD m => Reader TxLocalData  a -> m a
txRunQuery query = runReader query <$> getTxLocalData

-- | Convenient wrapper to run LocalUpdate in MonadSscLD.
txRunUpdate :: MonadTxLD m => State TxLocalData a -> m a
txRunUpdate upd = do
    (res, newLocalData) <- runState upd <$> getTxLocalData
    res <$ setTxLocalData newLocalData

getLocalTxs :: MonadTxLD m => m (HashSet (WithHash Tx))
getLocalTxs = txRunQuery getLocalTxsQ

removeLocalTx :: MonadTxLD m => WithHash Tx -> m ()
removeLocalTx tx = txRunUpdate . removeLocalTxU $ tx

txApplyGlobalUtxo :: MonadTxLD m => Utxo -> m ()
txApplyGlobalUtxo utxo = txRunUpdate $ applyGlobalUtxoU utxo

txLocalDataRollback :: MonadTxLD m => Word -> m ()
txLocalDataRollback toRollback = txRunUpdate $ txLocalDataRollbackU toRollback

txLocalDataProcessTx :: MonadTxLD m => WithHash Tx -> Utxo -> m ProcessTxRes
txLocalDataProcessTx tx utxo = txRunUpdate $ processTxU tx utxo

-- | Query returning '_txLocalTxs'
getLocalTxsQ :: Query (HashSet (WithHash Tx))
getLocalTxsQ = view txLocalTxs

-- | Add transaction to storage if it is fully valid.
processTxU :: WithHash Tx -> Utxo -> Update ProcessTxRes
processTxU tx utxo = do
    localSetSize <- use txLocalTxsSize
    if localSetSize < maxLocalTxs
        then processTxDo tx utxo
        else pure PTRoverwhelmed

processTxDo :: WithHash Tx -> Utxo -> Update ProcessTxRes
processTxDo tx utxo =
    ifM isKnown (pure PTRknown) $
    case verifyTxUtxo utxo tx' of
        VerSuccess -> do
            txLocalTxs %= HS.insert tx
            txLocalTxsSize += 1
            pure PTRadded
        VerFailure errors ->
            pure (mkPTRinvalid errors)
  where
    tx' = whData tx
    isKnown =
        or <$>
        sequence
            [ HS.member tx <$> use txLocalTxs
            , isJust . snd . LRU.lookup (whHash tx) <$> use txFilterCache
            ]

-- | Removes transaction from the local transaction set.
removeLocalTxU :: WithHash Tx -> Update ()
removeLocalTxU tx = do
    present <- HS.member tx <$> use txLocalTxs
    when present $ do
        txLocalTxs %= HS.delete tx
        txLocalTxsSize -= 1

-- | Insert transaction which is in block into filter cache.
cacheTx :: TxId -> Update ()
cacheTx txId = txFilterCache %= LRU.insert txId ()

txLocalDataRollbackU :: Word -> Update ()
txLocalDataRollbackU 0 = pass
txLocalDataRollbackU _ = invalidateCache

invalidateCache :: Update ()
invalidateCache = txFilterCache %= clearLRU

applyGlobalUtxoU :: Utxo -> Update ()
applyGlobalUtxoU globalUtxo = do
    localTxs <- uses txLocalTxs HS.toList
    let txs' = normalizeTxs localTxs globalUtxo
    txLocalTxs .= HS.fromList txs'
    txLocalTxsSize .= length txs'
    mapM_ (cacheTx . whHash) txs'
