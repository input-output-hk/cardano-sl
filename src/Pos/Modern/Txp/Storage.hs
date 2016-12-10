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

module Pos.Modern.Txp.Storage
       (
        -- txStorageFromUtxo
       -- , getUtxoByDepth
       -- , getUtxo
       -- , isTxVerified
       -- , txVerifyBlocks
       -- , txApplyBlocks
       -- , txRollback
       -- , processTx
       -- , filterLocalTxs
         txApplyBlocks
       , txRollback
       ) where
import           Control.Lens             (each, over, (^.), _1)
import           Control.Monad.IfElse     (aifM)
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromJust, isJust)
import           Database.RocksDB         (BatchOp (..), WriteBatch, write)
import           Formatting               (build, sformat, text, (%))
import           System.Wlog              (WithLogger, logError)
import           Universum

import           Pos.Crypto               (Hash, WithHash (..), hash, withHash)
import           Pos.Modern.State.Storage (DB (..),
                                           MonadDB (getBlockDB, getUndoDB, getUtxoDB),
                                           rocksGet, rocksGetRaw, rocksPutRaw)
import           Pos.Modern.Txp.RocksDB   (createDelTx, createPutTx)
import           Pos.Modern.Types.Utxo    (verifyTxs)
import           Pos.Ssc.Class.Types      (Ssc)
import           Pos.State.Storage.Types  (AltChain)
import           Pos.Types                (Block, BlockHeader, IdTxWitness, SlotId,
                                           Tx (..), TxIn (..), TxOut, TxWitness, Undo,
                                           blockSlot, blockTxws, blockTxws, convertFrom',
                                           headerHash, prevBlockL, slotIdF)

-- | Apply chain of /definitely/ valid blocks which go right after
-- last applied block. If invalid block is passed, this function will
-- panic.
txApplyBlocks :: (Ssc ssc, WithLogger m, MonadDB ssc m) => AltChain ssc -> m ()
txApplyBlocks blocks = do
    verdict <- txVerifyBlocks blocks
    case verdict of
        -- TODO Consider using `MonadError` and throwing `InternalError`.
        Left _ -> panic "Attempted to apply blocks that don't pass txVerifyBlocks"
        Right (txs, batches) -> do
            -- Apply all the blocks' transactions
            mapM_ (uncurry txApplyBlock)
                  (zipWith (\x->fmap Just . (x,)) (NE.toList blocks `zip` txs) batches)
            --writeTxOuts batch

txApplyBlock :: (Ssc ssc, MonadDB ssc m, WithLogger m)
             => (Block ssc, [IdTxWitness]) -> Maybe [BatchOp] -> m ()
txApplyBlock (b, txs) computedBatch = do
    when (not . isGenesisBlock $ b) $ do
        let hashPrevHeader = b ^. prevBlockL
        tip <- getTip
        if (isJust tip && hashPrevHeader == fromJust tip) then do
            -- SIMPLIFY IT!!!
            let batch = fromJust (computedBatch <|> (Just $ foldr' prependToBatch [] txs))
            writeTxOuts batch
            putTip (headerHash b)
        else
            logError "Error during application block to Undo DB: No TIP"
  where
    prependToBatch :: IdTxWitness -> [BatchOp] -> [BatchOp]
    prependToBatch (txId, (Tx{..}, _)) batch =
        let
            keys = zip (repeat txId) [(0::Word32)..]
            delIn = map (createDelTx . \x -> (txInHash x, txInIndex x)) txInputs -- simplify it
            putOut = map createPutTx $ zip keys txOutputs
        in
            foldr' (:) (foldr' (:) batch putOut) delIn --how we could simplify it?

-- | Rollback last @n@ blocks. This will replace current utxo to utxo
-- of desired depth block and also filter local transactions so they
-- can be applied. @tx@ prefix is used, because rollback may happen in
-- other storages as well.
txRollback :: (Ssc ssc, WithLogger m, MonadDB ssc m) => Word -> m ()
txRollback (fromIntegral -> n) = replicateM_ n txRollbackBlock

-- | Rollback last block
txRollbackBlock :: (Ssc ssc, WithLogger m, MonadDB ssc m) => m ()
txRollbackBlock =
    aifM getTip (\tip ->
        aifM (getBlock tip) (\block ->
            aifM (getUndo tip) (\undo -> do
                let txs = getTxs block
                --TODO more detailed message must be here
                unless (length undo == length txs) $ panic "Number of txs must be equal length of undo"
                let batchOrError = foldl' prependToBatch (Right []) $ zip txs undo
                case batchOrError of
                    Left msg    -> panic msg
                    Right batch -> writeTxOuts batch
                putTip (headerHash block))
            (errorMsg "No Undo for block")) -- should we use here panic, right?
            --(errorMsg $ sformat ("No Undo for block with hash: "%build) tip)
        (errorMsg "No Block"))
        --(errorMsg $ sformat ("No Block with hash: "%build) tip)
    (errorMsg "No TIP")
  where
    getTxs (Left _)   = []
    getTxs (Right mb) = map fst $ mb ^. blockTxws

    errorMsg msg = logError $ sformat ("Error during rollback block from Undo DB: "%text) msg

    prependToBatch :: Either Text [BatchOp] -> (Tx, [TxOut]) -> Either Text [BatchOp]
    prependToBatch er@(Left _) _ = er
    prependToBatch (Right batch) (tx@Tx{..}, undoTx) = do
        --TODO more detailed message must be here
        unless (length undoTx == length txInputs) $ Left "Number of txInputs must be equal length of undo"
        let txId = hash tx
            keys = zip (repeat txId) [(0::Word32)..]
            putIn = map (createPutTx .
                         \(txIn, undoIn) -> ((txInHash txIn, txInIndex txIn), undoIn)) -- simplify it
                    $ zip txInputs undoTx
            delOut = map createDelTx $ take (length txOutputs) keys
        return $ foldr' (:) (foldr' (:) batch putIn) delOut --how we could simplify it?

-- | Given number of blocks to rollback and some sidechain to adopt it
-- checks if it can be done prior to transaction validity. Returns a
-- list of topsorted transactions, head ~ deepest block on success.
txVerifyBlocks :: MonadDB ssc m => AltChain ssc
               -> m (Either Text ([[IdTxWitness]], [[BatchOp]]))
txVerifyBlocks newChain = do
    verifyRes <- foldM verifyDo (Right ([], [], M.empty)) newChainTxs
    return $
        case verifyRes of
          Left msg        -> Left msg
          Right ( accTxs
                , accBatch
                , _)      -> do
                Right (map convertFrom' . reverse $ accTxs, reverse accBatch)
  where
    newChainTxs :: [(SlotId, [(WithHash Tx, TxWitness)])]
    newChainTxs =
        map (\b -> (b ^. blockSlot,
                    over (each._1) withHash (b ^. blockTxws))) $
        rights (NE.toList newChain)

    -- verifyDo :: (Either Text([[(WithHash Tx, TxWitness)]], [BatchOp], Utxo))
    --          -> (SlotId, [(WithHash Tx, TxWitness)])
    --          -> m (Either Text ([[(WithHash Tx, TxWitness)]], [BatchOp], Utxo)) -- kaef
    verifyDo er@(Left _) _ = return er
    verifyDo (Right (accTxs, accBatch, localUtxo)) (slotId, txws) = do
        res <- verifyTxs txws localUtxo
        return $
            case res of
              Left reason         -> Left $ sformat eFormat slotId reason
              Right ( txws'
                    , batch'
                    , localUtxo') -> Right ( txws':accTxs
                                           , batch' : accBatch
                                           , localUtxo')
    eFormat =
        "Failed to apply transactions on block from slot " %
        slotIdF%", error: "%build

getTip :: MonadDB ssc m => m (Maybe (Hash (BlockHeader ssc)))
getTip = getUtxoDB >>= rocksGetRaw "tip"

putTip :: MonadDB ssc m => Hash (BlockHeader ssc) -> m ()
putTip h = getUtxoDB >>= rocksPutRaw "tip" h

getBlock :: (Ssc ssc, MonadDB ssc m) => Hash (BlockHeader ssc) -> m (Maybe (Block ssc))
getBlock h = getBlockDB >>= rocksGet h

getUndo :: (MonadDB ssc m) => Hash (BlockHeader ssc) -> m (Maybe Undo)
getUndo h = getUndoDB >>= rocksGet h

writeTxOuts :: MonadDB ssc m => WriteBatch -> m ()
writeTxOuts batch = do
    DB {..} <- getUtxoDB
    write rocksDb rocksWriteOpts batch

isGenesisBlock :: Block ssc -> Bool
isGenesisBlock (Left _) = True
isGenesisBlock _        = False

-- | Check if given transaction is verified, e. g.
-- is present in `k` and more blocks deeper
-- isTxVerified :: (Tx, TxWitness) -> Query Bool
-- isTxVerified tx = do
--     mutxo <- getUtxoByDepth k
--     case mutxo of
--         Nothing   -> pure False
--         Just utxo -> case verifyTxUtxo utxo tx of
--             VerSuccess   -> pure True
--             VerFailure _ -> pure False



-- processTx :: IdTxWitness -> Update ()
-- processTx = applyTx

-- | Normalize local transaction list -- throw away all transactions
-- that don't make sense anymore (e.g. after block application that
-- spends utxo we were counting on). Returns new transaction list,
-- sorted.
-- filterLocalTxs :: [IdTxWitness] -> Utxo -> [IdTxWitness]
-- filterLocalTxs localTxs = normalizeTxs' localTxs

-- | Takes the utxo we have now, reset it to head of utxo history and
-- apply all localtransactions we have. It applies @filterLocalTxs@
-- inside, because we can't apply transactions that don't apply.
-- Returns filtered localTransactions
-- overrideWithLocalTxs :: [IdTxWitness] -> Update ()
-- overrideWithLocalTxs localTxs = do
--     resetLocalUtxo
--     utxo <- use txUtxo
--     let txs = filterLocalTxs localTxs utxo
--     forM_ txs applyTx

