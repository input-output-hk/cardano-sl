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
import qualified Data.HashMap.Strict     as HM
import           Serokell.Util           (VerificationRes (..))
import           Universum

import           Pos.Constants           (maxLocalTxs)
import           Pos.State.Storage.Types (ProcessTxRes (..), mkPTRinvalid)
import           Pos.Types               (IdTxWitness, Tx (..), TxId, TxWitness, Utxo,
                                          normalizeTxs', verifyTxUtxo)
import           Pos.Util                (clearLRU)

type TxMap = HashMap TxId (Tx, TxWitness)

data TxLocalData = TxLocalData
    { -- | Local set of transactions. These are valid (with respect to
      -- the head of last block's utxo) transactions which are known
      -- to the node and are /not/ included in the blockchain store by
      -- the node. This set is used later to form the block payload.
      _txLocalTxs     :: !TxMap
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

getLocalTxs :: MonadTxLD m => m TxMap
getLocalTxs = txRunQuery getLocalTxsQ

removeLocalTx :: MonadTxLD m => IdTxWitness -> m ()
removeLocalTx tx = txRunUpdate . removeLocalTxU . fst $ tx

txApplyGlobalUtxo :: MonadTxLD m => Utxo -> m ()
txApplyGlobalUtxo utxo = txRunUpdate $ applyGlobalUtxoU utxo

txLocalDataRollback :: MonadTxLD m => Word -> m ()
txLocalDataRollback toRollback = txRunUpdate $ txLocalDataRollbackU toRollback

txLocalDataProcessTx :: MonadTxLD m => IdTxWitness -> Utxo -> m ProcessTxRes
txLocalDataProcessTx tx utxo = txRunUpdate $ processTxU tx utxo

-- | Query returning '_txLocalTxs'
getLocalTxsQ :: Query TxMap
getLocalTxsQ = view txLocalTxs

-- | Add transaction to storage if it is fully valid.
processTxU :: IdTxWitness -> Utxo -> Update ProcessTxRes
processTxU tx utxo = do
    localSetSize <- use txLocalTxsSize
    if localSetSize < maxLocalTxs
        then processTxDo tx utxo
        else pure PTRoverwhelmed

processTxDo :: IdTxWitness -> Utxo -> Update ProcessTxRes
processTxDo (id, tx) utxo =
    ifM isKnown (pure PTRknown) $
    case verifyTxUtxo utxo tx of
        VerSuccess -> do
            txLocalTxs %= HM.insert id tx
            txLocalTxsSize += 1
            pure PTRadded
        VerFailure errors ->
            pure (mkPTRinvalid errors)
  where
    isKnown =
        or <$>
        sequence
            [ HM.member id <$> use txLocalTxs
            , isJust . snd . LRU.lookup id <$> use txFilterCache
            ]

-- | Removes transaction from the local transaction set.
removeLocalTxU :: TxId -> Update ()
removeLocalTxU id = do
    present <- HM.member id <$> use txLocalTxs
    when present $ do
        txLocalTxs %= HM.delete id
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
    localTxs <- uses txLocalTxs HM.toList
    let txs' = normalizeTxs' localTxs globalUtxo
    txLocalTxs .= HM.fromList txs'
    txLocalTxsSize .= length txs'
    mapM_ cacheTx (map fst txs')
