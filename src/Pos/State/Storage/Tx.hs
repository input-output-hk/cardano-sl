{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
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
       , txVerifyBlocks

       , processTx
       , txApplyBlocks
       , txRollback
       ) where

import           Control.Lens            (ix, makeClassy, preview, use, uses, view, (%=),
                                          (+=), (-=), (.=), (<~), (^.))
import qualified Data.Cache.LRU          as LRU
import qualified Data.HashSet            as HS
import qualified Data.List.NonEmpty      as NE
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Formatting              (build, int, sformat, (%))
import           Serokell.Util           (VerificationRes (..), isVerSuccess)
import           Universum

import           Pos.Constants           (maxLocalTxs)
import           Pos.Crypto              (hash)
import           Pos.State.Storage.Types (AltChain, ProcessTxRes (..), mkPTRinvalid)
import           Pos.Types               (Block, SlotId, Tx (..), TxId, Utxo,
                                          applyTxToUtxo, blockSlot, blockTxs, slotIdF,
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
      -- | @length@ is @O(n)@ for 'HE.HashSet' so we store it explicitly.
    , _txLocalTxsSize :: !Int
    , -- | Set of local unspent transaction outputs. It is need to
      -- check new transactions and run follow-the-satoshi, for
      -- example.
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

-- | Given number of blocks to rollback @k@ and alternative blockchain
-- this function tries to check if it's possible to switch to a new
-- branch @k@ blocks deep by appending alternative chain to that
-- history point.
txVerifyBlocks :: Word -> AltChain ssc -> Query VerificationRes
txVerifyBlocks (fromIntegral -> toRollback) newChain = do
    mUtxo <- preview (txUtxoHistory . ix toRollback)
    return $ case mUtxo of
      Nothing ->
        VerFailure [sformat ("Can't rollback on "%int%" blocks") toRollback]
      Just utxo ->
        case foldM verifyDo utxo newChainTxs of
          Right _ -> VerSuccess
          Left es -> VerFailure es
  where
    newChainTxs =
        mconcat .
        fmap (\b -> fmap (b ^. blockSlot,) . toList $ b ^. blockTxs) . rights $
        NE.toList newChain
    verifyDo :: Utxo -> (SlotId, Tx) -> Either [Text] Utxo
    verifyDo utxo (slotId, tx) =
        case verifyTxUtxo utxo tx of
            VerSuccess    -> Right $ applyTxToUtxo tx utxo
            VerFailure es -> Left $ map (sformat eFormat tx slotId) es
    eFormat =
        "Failed to apply transaction ("%build%") on block from slot " %
        slotIdF%", error: "%build

-- | Get utxo corresponding to state right after block with given
-- depth has been applied.
getUtxoByDepth :: Word -> Query (Maybe Utxo)
getUtxoByDepth (fromIntegral -> depth) = preview $ txUtxoHistory . ix depth

type Update a = forall m x. (HasTxStorage x, MonadState x m) => m a

-- | Add transaction to storage if it is fully valid.
processTx :: Tx -> Update ProcessTxRes
processTx tx = do
    localSetSize <- use txLocalTxsSize
    if localSetSize < maxLocalTxs
        then processTxDo tx
        else return PTRoverwhelmed

processTxDo :: Tx -> Update ProcessTxRes
processTxDo tx =
    ifM isKnown (pure PTRknown) $
    do verRes <- verifyTx tx
       case verRes of
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
verifyTx tx = flip verifyTxUtxo tx <$> use txUtxo

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

-- | Apply chain of definitely valid blocks which go right after last
-- applied block.
txApplyBlocks :: AltChain ssc -> Update ()
txApplyBlocks blocks = do
    mapM_ txApplyBlock blocks
    filterLocalTxs

txApplyBlock :: Block ssc -> Update ()
txApplyBlock (Left _) = do
    utxo <- use txUtxo
    txUtxoHistory %= (utxo:)
txApplyBlock (Right mainBlock) = do
    let txs = mainBlock ^. blockTxs
    mapM_ applyTx txs
    mapM_ removeLocalTx txs
    mapM_ cacheTx txs
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
    filterLocalTxs
    invalidateCache
  where
    -- Consider using `MonadError` and throwing `InternalError`.
    onError = (panic "attempt to rollback to too old or non-existing block")

filterLocalTxs :: Update ()
filterLocalTxs = do
    txs  <- uses txLocalTxs toList
    txs' <- HS.fromList <$> filterM (fmap isVerSuccess . verifyTx) txs
    txLocalTxs     .= txs'
    txLocalTxsSize .= length txs'

invalidateCache :: Update ()
invalidateCache = txFilterCache %= clearLRU
