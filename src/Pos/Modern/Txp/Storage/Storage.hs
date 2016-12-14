{-# LANGUAGE ConstraintKinds       #-}
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

module Pos.Modern.Txp.Storage.Storage
       (
         txVerifyBlocks
       , txApplyBlocks
       , processTx
       , txRollbackBlocks
       ) where
import           Control.Lens                    (each, over, (^.), _1)
import           Control.Monad.IfElse            (aifM)
import qualified Data.HashMap.Strict             as HM
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe                      (fromJust, isJust)
import           Formatting                      (build, sformat, text, (%))
import           Serokell.Util                   (VerificationRes (..))
import           System.Wlog                     (WithLogger, logError)
import           Universum

import           Pos.Constants                   (maxLocalTxs)
import           Pos.Crypto                      (WithHash (..), hash, withHash)
import           Pos.Modern.DB                   (MonadDB, getUtxoDB)
import           Pos.Modern.DB.Block             (getBlock, getUndo)
import           Pos.Modern.DB.Utxo              (BatchOp (..), getTip, putTip,
                                                  writeBatchToUtxo)
import           Pos.Modern.Txp.Class            (MonadTxpLD (..), MonadUtxo)
import           Pos.Modern.Txp.Holder           (runTxpLDHolder)
import           Pos.Modern.Txp.Storage.MemPool  (MemPool (..))
import qualified Pos.Modern.Txp.Storage.UtxoView as UV (createFromDB)
import           Pos.Modern.Types.Tx             (topsortTxs)
import           Pos.Modern.Types.Utxo           (applyTxToUtxo', verifyTxUtxo, verifyTxs)
import           Pos.Ssc.Class.Types             (Ssc)
import           Pos.State.Storage.Types         (AltChain, ProcessTxRes (..),
                                                  mkPTRinvalid)
import           Pos.Types                       (Block, IdTxWitness, SlotId, Tx (..),
                                                  TxIn (..), TxOut, TxWitness, blockSlot,
                                                  blockTxws, blockTxws, convertFrom',
                                                  headerHash, prevBlockL, slotIdF)

type TxpWorkMode ssc m = (Ssc ssc, WithLogger m, MonadDB ssc m, MonadTxpLD ssc m, MonadUtxo ssc m)

-- | Apply chain of /definitely/ valid blocks which go right after
-- last applied block. If invalid block is passed, this function will
-- panic.
txApplyBlocks :: TxpWorkMode ssc m => AltChain ssc -> m ()
txApplyBlocks blocks = do
    verdict <- txVerifyBlocks blocks
    case verdict of
        -- TODO Consider using `MonadError` and throwing `InternalError`.
        Left _ -> panic "Attempted to apply blocks that don't pass txVerifyBlocks"
        Right txs -> do
            -- Apply all the blocks' transactions
            -- TODO actually, we can improve it: we can use UtxoView from txVerifyBlocks
            -- Now we recalculate TxIn which must be removed from Utxo DB or added to Utxo DB
            -- I can improve it, if it is bottlneck

            -- We apply all blocks and filter mempool for every block
            mapM_ (uncurry txApplyBlock)
                  (zip (NE.toList blocks `zip` txs) (repeat Nothing))
            getUtxoDB >>= setUtxoView . UV.createFromDB -- reset utxo view before normalization
            normalizeTxpLD

txApplyBlock :: TxpWorkMode ssc m
             => (Block ssc, [IdTxWitness]) -> Maybe [BatchOp] -> m ()
txApplyBlock (b, txs) computedBatch = do
    when (not . isGenesisBlock $ b) $ do
        let hashPrevHeader = b ^. prevBlockL
        tip <- getTip
        if (isJust tip && hashPrevHeader == fromJust tip) then do
            -- SIMPLIFY IT!!!
            let batch = fromJust (computedBatch <|>
                                 (Just $ foldr' prependToBatch [] txs))
            filterMemPool txs
            writeBatchToUtxo batch
            putTip (headerHash b)
        else
            logError "Error during application of block to Undo DB: No TIP"
  where
    prependToBatch :: IdTxWitness -> [BatchOp] -> [BatchOp]
    prependToBatch (txId, (Tx{..}, _)) batch =
        let
            keys = zipWith TxIn (repeat txId) [0..]
            delIn = map DelTxIn txInputs
            putOut = map (uncurry AddTxOut) $ zip keys txOutputs
        in
            foldr' (:) (foldr' (:) batch putOut) delIn --how we could simplify it?

-- | Given number of blocks to rollback and some sidechain to adopt it
-- checks if it can be done prior to transaction validity. Returns a
-- list of topsorted transactions, head ~ deepest block on success.
txVerifyBlocks :: MonadDB ssc m => AltChain ssc
               -> m (Either Text [[IdTxWitness]])
txVerifyBlocks newChain = do
    utxoDB <- getUtxoDB
    verifyRes <- runTxpLDHolder
                     (foldM verifyDo (Right []) newChainTxs)
                     (UV.createFromDB utxoDB)
    return $
        case verifyRes of
          Left msg                    -> Left msg
          Right accTxs ->
                Right (map convertFrom' . reverse $ accTxs)
  where
    newChainTxs :: [(SlotId, [(WithHash Tx, TxWitness)])]
    newChainTxs =
        map (\b -> (b ^. blockSlot,
                    over (each._1) withHash (b ^. blockTxws))) $
        rights (NE.toList newChain)

    -- verifyDo  :: (Either Text [[IdTxWitness]])
    --           -> (SlotId, [(WithHash Tx, TxWitness)])
    --           -> m (Either Text [[IdTxWitness]])
    verifyDo er@(Left _) _ = return er
    verifyDo (Right accTxs) (slotId, txws) = do
        res <- verifyTxs txws
        return $
            case res of
              Left reason -> Left  (sformat eFormat slotId reason)
              Right txws' -> Right (txws':accTxs)
    eFormat =
        "Failed to apply transactions on block from slot " %
        slotIdF%", error: "%build

processTx :: (MonadDB ssc m, MonadTxpLD ssc m, MonadUtxo ssc m)
          => IdTxWitness -> m ProcessTxRes
processTx tx = do
    localSize <- localTxsSize <$> getMemPool
    if localSize < maxLocalTxs
        then processTxDo tx
        else pure PTRoverwhelmed

-- | We need lock here!
processTxDo :: (MonadDB ssc m, MonadTxpLD ssc m, MonadUtxo ssc m)
            => IdTxWitness -> m ProcessTxRes
processTxDo itw@(id, tx) =
    ifM isKnown (pure PTRknown) $ do
    verifyRes <- verifyTxUtxo tx
    case verifyRes of
        VerSuccess -> do
            mp <- getMemPool
            let locTxs = localTxs mp
                locTxsSize = localTxsSize mp
            applyTxToUtxo' itw
            setMemPool $ MemPool (HM.insert id tx locTxs) (locTxsSize + 1)
            pure PTRadded
        VerFailure errors ->
            pure (mkPTRinvalid errors)
  where
    isKnown =
        or <$>
        sequence
            [ HM.member id <$> (localTxs <$> getMemPool)
--            , isJust . snd . LRU.lookup id <$> use txFilterCache
            ]

-- | Rollback last @n@ blocks. This will replace current utxo to utxo
-- of desired depth block and also filter local transactions so they
-- can be applied. @tx@ prefix is used, because rollback may happen in
-- other storages as well.
txRollbackBlocks :: (Ssc ssc, WithLogger m, MonadDB ssc m) => Word -> m ()
txRollbackBlocks (fromIntegral -> n) = replicateM_ n txRollbackBlock

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
                    Right batch -> writeBatchToUtxo batch
                    -- If we store block cache in UtxoView we must invalidate it
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
            keys = zipWith TxIn (repeat txId) [0..]
            putIn = map (uncurry AddTxOut) $ zip txInputs undoTx
            delOut = map DelTxIn $ take (length txOutputs) keys
        return $ foldr' (:) (foldr' (:) batch putIn) delOut --how we could simplify it?

-- | Remove from mem pool transactions from block
filterMemPool :: MonadTxpLD ssc m => [IdTxWitness]  -> m ()
filterMemPool blockTxs = do
    mp <- getMemPool
    let newMPTxs = (localTxs mp) `HM.difference` (HM.fromList blockTxs)
    setMemPool $ MemPool newMPTxs (HM.size newMPTxs)

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Removed from MemPool invalid transactions
normalizeTxpLD :: (MonadTxpLD ssc m, MonadUtxo ssc m) => m ()
normalizeTxpLD  = do
    mpTxs <- HM.toList . localTxs <$> getMemPool
    maybe (setMemPool $ MemPool HM.empty 0)
          (\topsorted -> do
              validTxs <- foldlM canApply [] topsorted
              setMemPool $ MemPool (HM.fromList validTxs) (length validTxs)
          ) (topsortTxs (\(i, (t, _)) -> WithHash t i) mpTxs)
  where
--    canApply :: [IdTxWitness] -> IdTxWitness -> m [IdTxWitness]
    canApply xs itw@(_, (tx, txw)) = do
        verifyRes <- verifyTxUtxo (tx, txw)
        case verifyRes of
            VerSuccess   -> return (itw : xs)
            VerFailure _ -> return xs

isGenesisBlock :: Block ssc -> Bool
isGenesisBlock (Left _) = True
isGenesisBlock _        = False

