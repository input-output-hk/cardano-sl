{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Internal state of the transaction-handling worker. Trasnaction
-- processing logic.

module Pos.State.Storage.Tx
       (
         TxStorage (..)
       , HasTxStorage (txStorage)
       , txStorageFromUtxo

       , getLocalTxs
       , getUtxoByDepth
       , isTxVerified
       , txVerifyBlocks

       , processTx
       , txApplyBlocks
       , txRollback
       ) where

import           Control.Lens            (ix, makeClassy, preview, use, uses, view, (%=),
                                          (+=), (-=), (.=), (<&>), (<~), (^.))
import qualified Data.Cache.LRU          as LRU
import qualified Data.HashSet            as HS
import qualified Data.List.NonEmpty      as NE
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Formatting              (build, int, sformat, (%))
import           Serokell.Util           (VerificationRes (..))
import           Universum

import           Pos.Constants           (k, maxLocalTxs)
import           Pos.Crypto              (hash)
import           Pos.State.Storage.Types (AltChain, ProcessTxRes (..), mkPTRinvalid)
import           Pos.Types               (Block, SlotId, Tx (..), TxId, Utxo,
                                          applyTxToUtxo, blockSlot, blockTxs,
                                          normalizeTxs, slotIdF, verifyAndApplyTxs,
                                          verifyTxUtxo)
import           Pos.Util                (clearLRU)

-- | Transaction-related state part, includes transactions, utxo and
-- auxiliary structures needed for transaction processing.
data TxStorage = TxStorage
    { -- | Local set of transactions. These are valid (with respect to
      -- the head of last block's utxo) transactions which are known
      -- to the node and are /not/ included in the blockchain store by
      -- the node. This set is used later to form the block payload.
      _txLocalTxs     :: !(HashSet Tx)
    , -- | @length@ is @O(n)@ for 'HE.HashSet' so we store it explicitly.
      _txLocalTxsSize :: !Int
    , -- | Set of unspent transaction outputs formed by applying
      -- txLocalTxs to the head of txUtxoHistory. It is need to check
      -- new transactions and run follow-the-satoshi, for example.
      _txUtxo         :: !Utxo
    , -- | History of utxo. May be necessary in case of
      -- reorganization. Also it is needed for MPC. Head of this list
      -- is utxo corresponding to last known block.
      _txUtxoHistory  :: ![Utxo]
    , -- | Transactions recently added to blocks. Used for ignoring
      -- overhead when same transaction is propagated several times.
      _txFilterCache  :: !(LRU.LRU TxId ())
    }

-- | Classy lens generated for 'TxStorage'
makeClassy ''TxStorage
deriveSafeCopySimple 0 'base ''TxStorage

-- | Generate TxStorage from non-default utxo.
txStorageFromUtxo :: Utxo -> TxStorage
txStorageFromUtxo u =
    TxStorage
    { _txLocalTxs = mempty
    , _txLocalTxsSize = 0
    , _txUtxo = u
    , _txUtxoHistory = [u]
    , _txFilterCache = LRU.newLRU (Just 10000)
    }

type Query a = forall m x. (HasTxStorage x, MonadReader x m) => m a

-- | Query returning '_txLocalTxs'
getLocalTxs :: Query (HashSet Tx)
getLocalTxs = view txLocalTxs

-- | Given number of blocks to rollback and some sidechain to adopt it
-- checks if it can be done prior to transaction validity. Returns a
-- list of topsorted transactions, head ~ deepest block on success.
txVerifyBlocks :: Word -> AltChain ssc -> Query (Either Text [[Tx]])
txVerifyBlocks (fromIntegral -> toRollback) newChain = do
    (preview (txUtxoHistory . ix toRollback)) <&> \case
        Nothing ->
            Left $ sformat ("Can't rollback on "%int%" blocks") toRollback
        Just utxo -> reverse . snd <$> foldM verifyDo (utxo, []) newChainTxs
  where
    newChainTxs :: [(SlotId,[Tx])]
    newChainTxs =
        fmap (\b -> (b ^. blockSlot, toList $ b ^. blockTxs)) . rights $
        NE.toList newChain
    verifyDo :: (Utxo,[[Tx]]) -> (SlotId, [Tx]) -> Either Text (Utxo, [[Tx]])
    verifyDo (utxo,accTxs) (slotId, txs) =
        case verifyAndApplyTxs txs utxo of
          Left reason        -> Left $ sformat eFormat slotId reason
          Right (txs',utxo') -> Right (utxo',txs':accTxs)
    eFormat =
        "Failed to apply transactions on block from slot " %
        slotIdF%", error: "%build

-- | Get utxo corresponding to state right after block with given
-- depth has been applied.
getUtxoByDepth :: Word -> Query (Maybe Utxo)
getUtxoByDepth (fromIntegral -> depth) = preview $ txUtxoHistory . ix depth

-- | Check if given transaction is verified, e. g.
-- is present in `k` and more blocks deeper
isTxVerified :: Tx -> Query Bool
isTxVerified tx = do
    mutxo <- getUtxoByDepth k
    case mutxo of
        Nothing   -> pure False
        Just utxo -> case verifyTxUtxo utxo tx of
            VerSuccess   -> pure True
            VerFailure _ -> pure False

type Update a = forall m x. (HasTxStorage x, MonadState x m) => m a

-- | Add transaction to storage if it is fully valid.
processTx :: Tx -> Update ProcessTxRes
processTx tx = do
    localSetSize <- use txLocalTxsSize
    if localSetSize < maxLocalTxs
        then processTxDo tx
        else pure PTRoverwhelmed

processTxDo :: Tx -> Update ProcessTxRes
processTxDo tx =
    ifM isKnown (pure PTRknown) $
    verifyTx tx >>= \case
        VerSuccess -> do
            txLocalTxs %= HS.insert tx
            txLocalTxsSize += 1
            applyTx tx
            pure PTRadded
        VerFailure errors ->
            pure (mkPTRinvalid errors)
  where
    isKnown =
        or <$>
        sequence
            [ HS.member tx <$> use txLocalTxs
            , isJust . snd . LRU.lookup (hash tx) <$> use txFilterCache
            ]

-- | Checks if it's possible to apply transaction to current local
-- utxo.
verifyTx :: Tx -> Update VerificationRes
verifyTx tx = uses txUtxo $ flip verifyTxUtxo tx

-- | Applies transaction to current utxo. Should be called only if
-- it's possible to do so (see 'verifyTx').
applyTx :: Tx -> Update ()
applyTx tx = txUtxo %= applyTxToUtxo tx

-- | Removes transaction from the local transaction set.
removeLocalTx :: Tx -> Update ()
removeLocalTx tx = do
    present <- HS.member tx <$> use txLocalTxs
    when present $ do
        txLocalTxs %= HS.delete tx
        txLocalTxsSize -= 1

-- | Insert transaction which is in block into filter cache.
cacheTx :: Tx -> Update ()
cacheTx (hash -> txId) = txFilterCache %= LRU.insert txId ()

-- | Apply chain of /definitely/ valid blocks which go right after
-- last applied block. If invalid block is passed, this function will
-- panic.
txApplyBlocks :: AltChain ssc -> Update ()
txApplyBlocks blocks = do
    verdict <- runReaderT (txVerifyBlocks 0 blocks) =<< use txStorage
    case verdict of
        -- TODO Consider using `MonadError` and throwing `InternalError`.
        Left _ -> panic "Attempted to apply blocks that don't pass txVerifyBlocks"
        Right txs -> do
            -- Reset utxo to the last block's utxo. Doesn't change
            -- localTxs
            resetLocalUtxo
            -- Apply all the blocks' transactions
            mapM_ txApplyBlock (NE.toList blocks `zip` txs)
            -- It also can be that both transaction X ∈ localStorage
            -- and Y ∈ block spend output A, so we must filter local
            -- transactions that became invalid after block
            -- application and regenerate local utxo with them
            overrideWithLocalTxs

txApplyBlock :: (Block ssc, [Tx]) -> Update ()
txApplyBlock (Left _, _) = do
    utxo <- use txUtxo
    txUtxoHistory %= (utxo:)
txApplyBlock (_, txs) = do
    mapM_ applyTx txs
    -- As far as cache contains only those transactions that were
    -- included into local transactions set, even in case we delete
    -- more transactions from local storage then txs (see
    -- overrideWithLocalTxs usage in txApplyBlocks), it should be okay
    -- not to invalidate them because their inputs are used already.
    mapM_ cacheTx txs
    mapM_ removeLocalTx txs
    utxo <- use txUtxo
    txUtxoHistory %= (utxo:)

-- | Rollback last @n@ blocks. This will replace current utxo to utxo
-- of desired depth block and also filter local transactions so they
-- can be applied. @tx@ prefix is used, because rollback may happen in
-- other storages as well.
txRollback :: Word -> Update ()
txRollback 0 = pass
txRollback (fromIntegral -> n) = do
    txUtxo <~ fromMaybe onError . (`atMay` n) <$> use txUtxoHistory
    txUtxoHistory %= drop n
    overrideWithLocalTxs
    invalidateCache
  where
    -- TODO Consider using `MonadError` and throwing `InternalError`.
    onError = (panic "attempt to rollback to too old or non-existing block")

-- | Normalize local transaction list -- throw away all transactions
-- that don't make sense anymore (e.g. after block application that
-- spends utxo we were counting on). Returns new transaction list,
-- sorted.
filterLocalTxs :: Update [Tx]
filterLocalTxs = do
    txs <- uses txLocalTxs toList
    utxo <- use txUtxo
    let txs' = normalizeTxs txs utxo
    txLocalTxs .= HS.fromList txs'
    txLocalTxsSize .= length txs'
    pure txs'

-- | Takes the utxo we have now, reset it to head of utxo history and
-- apply all localtransactions we have. It applies @filterLocalTxs@
-- inside, because we can't apply transactions that don't apply.
overrideWithLocalTxs :: Update ()
overrideWithLocalTxs = do
    resetLocalUtxo
    txs <- filterLocalTxs
    forM_ txs applyTx

-- | Erases local utxo and puts utxo of the last block on it's place.
resetLocalUtxo :: Update ()
resetLocalUtxo = do
    headUtxo <- uses txUtxoHistory head
    whenJust headUtxo $ \h -> txUtxo .= h

invalidateCache :: Update ()
invalidateCache = txFilterCache %= clearLRU
