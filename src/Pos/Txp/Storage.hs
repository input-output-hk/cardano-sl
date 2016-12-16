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

module Pos.Txp.Storage
       (
         TxStorage (..)
       , HasTxStorage (txStorage)
       , txStorageFromUtxo

       , getUtxoByDepth
       , getUtxo
       , getOldestUtxo
       , isTxVerified
       , txVerifyBlocks
       , txApplyBlocks
       , txRollback
       , processTx
       , filterLocalTxs
       ) where

import           Control.Lens            (each, ix, makeClassy, over, preview, to, use,
                                          uses, view, (%=), (.=), (<&>), (<~), (^.), _1)
import           Data.List               (last)
import qualified Data.List.NonEmpty      as NE
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Formatting              (build, int, sformat, (%))
import           Serokell.Util           (VerificationRes (..))
import           Universum

import           Pos.Constants           (k)
import           Pos.Crypto              (WithHash (..), withHash)
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types               (Block, IdTxWitness, SlotId, Tx (..), TxWitness,
                                          Utxo, applyTxToUtxoPure', blockSlot, blockTxws,
                                          convertFrom', normalizeTxsPure', slotIdF,
                                          verifyAndApplyTxsPure, verifyTxUtxoPure)

-- | Transaction-related state part, includes transactions, utxo and
-- auxiliary structures needed for transaction processing.
data TxStorage = TxStorage
    {
      -- | History of utxo. May be necessary in case of
      -- reorganization. Also it is needed for MPC. Head of this list
      -- is utxo corresponding to last known block.
      _txUtxoHistory :: ![Utxo]
    , -- | Set of unspent transaction outputs formed by applying
      -- txLocalTxs to the head of txUtxoHistory. It is need to check
      -- new transactions and run follow-the-satoshi, for example.
      _txUtxo        :: !Utxo
    }

-- | Generate TxStorage from non-default utxo.
txStorageFromUtxo :: Utxo -> TxStorage
txStorageFromUtxo u = TxStorage [u] u

-- | Classy lens generated for 'TxStorage'
makeClassy ''TxStorage
deriveSafeCopySimple 0 'base ''TxStorage

type Query a = forall m x. (HasTxStorage x, MonadReader x m) => m a

-- | Applies transaction to current utxo. Should be called only if
-- it's possible to do so (see 'verifyTx').
applyTx :: IdTxWitness -> Update ()
applyTx tx = txUtxo %= applyTxToUtxoPure' tx

-- | Given number of blocks to rollback and some sidechain to adopt it
-- checks if it can be done prior to transaction validity. Returns a
-- list of topsorted transactions, head ~ deepest block on success.
txVerifyBlocks :: Word -> AltChain ssc -> Query (Either Text [[IdTxWitness]])
txVerifyBlocks (fromIntegral -> toRollback) newChain = do
    (preview (txUtxoHistory . ix toRollback)) <&> \case
        Nothing ->
            Left $ sformat ("Can't rollback on "%int%" blocks") toRollback
        Just utxo -> map convertFrom' . reverse . snd <$> foldM verifyDo (utxo, []) newChainTxs
  where
    newChainTxs :: [(SlotId, [(WithHash Tx, TxWitness)])]
    newChainTxs =
        map (\b -> (b ^. blockSlot,
                    over (each._1) withHash (b ^. blockTxws))) $
        rights (NE.toList newChain)
    verifyDo :: (Utxo, [[(WithHash Tx, TxWitness)]])
             -> (SlotId, [(WithHash Tx, TxWitness)])
             -> Either Text (Utxo, [[(WithHash Tx, TxWitness)]])
    verifyDo (utxo, accTxs) (slotId, txws) =
        case verifyAndApplyTxsPure txws utxo of
          Left reason         -> Left $ sformat eFormat slotId reason
          Right (txws',utxo') -> Right (utxo',txws':accTxs)
    eFormat =
        "Failed to apply transactions on block from slot " %
        slotIdF%", error: "%build

getUtxo :: Query Utxo
getUtxo = view txUtxo

-- | Get utxo corresponding to state right after block with given
-- depth has been applied.
getUtxoByDepth :: Word -> Query (Maybe Utxo)
getUtxoByDepth (fromIntegral -> depth) = preview $ txUtxoHistory . ix depth

-- | Get the very first (genesis) utxo.
getOldestUtxo :: Query Utxo
getOldestUtxo = view $ txUtxoHistory . to last

-- | Check if given transaction is verified, e. g.
-- is present in `k` and more blocks deeper
isTxVerified :: (Tx, TxWitness) -> Query Bool
isTxVerified tx = do
    mutxo <- getUtxoByDepth k
    case mutxo of
        Nothing   -> pure False
        Just utxo -> case verifyTxUtxoPure utxo tx of
            VerSuccess   -> pure True
            VerFailure _ -> pure False

type Update a = forall m x. (HasTxStorage x, MonadState x m) => m a

-- | Apply chain of /definitely/ valid blocks which go right after
-- last applied block. If invalid block is passed, this function will
-- panic.
txApplyBlocks :: [IdTxWitness] -> AltChain ssc -> Update ()
txApplyBlocks localTxs blocks = do
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
            overrideWithLocalTxs localTxs

txApplyBlock :: (Block ssc, [IdTxWitness]) -> Update ()
txApplyBlock (b, txs) = do
    case b of
      Left _ -> return ()
      _      -> mapM_ applyTx txs
    utxo <- use txUtxo
    txUtxoHistory %= (utxo:)

-- | Rollback last @n@ blocks. This will replace current utxo to utxo
-- of desired depth block and also filter local transactions so they
-- can be applied. @tx@ prefix is used, because rollback may happen in
-- other storages as well.
txRollback :: [IdTxWitness] -> Word -> Update ()
txRollback _ 0 = pass
txRollback localTxs (fromIntegral -> n) = do
    txUtxo <~ fromMaybe onError . (`atMay` n) <$> use txUtxoHistory
    txUtxoHistory %= drop n
    overrideWithLocalTxs localTxs
  where
    -- TODO Consider using `MonadError` and throwing `InternalError`.
    onError = (panic "attempt to rollback to too old or non-existing block")

processTx :: IdTxWitness -> Update ()
processTx = applyTx

-- | Erases local utxo and puts utxo of the last block on it's place.
resetLocalUtxo :: Update ()
resetLocalUtxo = do
    headUtxo <- uses txUtxoHistory head
    whenJust headUtxo $ \h -> txUtxo .= h

-- | Normalize local transaction list -- throw away all transactions
-- that don't make sense anymore (e.g. after block application that
-- spends utxo we were counting on). Returns new transaction list,
-- sorted.
filterLocalTxs :: [IdTxWitness] -> Utxo -> [IdTxWitness]
filterLocalTxs localTxs = normalizeTxsPure' localTxs

-- | Takes the utxo we have now, reset it to head of utxo history and
-- apply all localtransactions we have. It applies @filterLocalTxs@
-- inside, because we can't apply transactions that don't apply.
-- Returns filtered localTransactions
overrideWithLocalTxs :: [IdTxWitness] -> Update ()
overrideWithLocalTxs localTxs = do
    resetLocalUtxo
    utxo <- use txUtxo
    let txs = filterLocalTxs localTxs utxo
    forM_ txs applyTx
