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
import qualified Data.HashSet                    as HS
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe                      (fromJust)
import           Formatting                      (build, sformat, text, (%))
import           Serokell.Util                   (VerificationRes (..))
import           System.Wlog                     (WithLogger, logError)
import           Universum

import           Pos.Constants                   (maxLocalTxs)
import           Pos.Crypto                      (WithHash (..), hash, withHash)
import           Pos.Modern.DB                   (DB, MonadDB, getUtxoDB)
import           Pos.Modern.DB.Block             (getBlock, getUndo)
import           Pos.Modern.DB.Utxo              (BatchOp (..), getTip, writeBatchToUtxo)
import           Pos.Modern.Txp.Class            (MonadTxpLD (..), TxpLD)
import           Pos.Modern.Txp.Holder           (TxpLDHolder, runTxpLDHolderUV)
import           Pos.Modern.Txp.Storage.Types    (MemPool (..), UtxoView (..))
import qualified Pos.Modern.Txp.Storage.UtxoView as UV
import           Pos.Ssc.Class.Types             (Ssc)
import           Pos.State.Storage.Types         (AltChain, ProcessTxRes (..),
                                                  mkPTRinvalid)
import           Pos.Types                       (Block, IdTxWitness, MonadUtxo,
                                                  MonadUtxoRead (utxoGet), SlotId,
                                                  Tx (..), TxIn (..), TxOut, TxWitness,
                                                  applyTxToUtxo', blockSlot, blockTxws,
                                                  blockTxws, convertFrom', headerHash,
                                                  prevBlockL, slotIdF, topsortTxs,
                                                  verifyTxPure)
import           Pos.Types.Utxo                  (verifyAndApplyTxs, verifyTxUtxo)

type TxpWorkMode ssc m = ( Ssc ssc
                         , WithLogger m
                         , MonadDB ssc m
                         , MonadTxpLD ssc m
                         , MonadUtxo m
                         , MonadThrow m)

type MinTxpWorkMode ssc m = (
                              MonadDB ssc m
                            , MonadTxpLD ssc m
                            , MonadUtxo m
                            , MonadThrow m)
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
            normalizeTxpLD

txApplyBlock :: TxpWorkMode ssc m
             => (Block ssc, [IdTxWitness]) -> Maybe [BatchOp ssc] -> m ()
txApplyBlock (b, txs) computedBatch = do
    when (not . isGenesisBlock $ b) $ do
        let hashPrevHeader = b ^. prevBlockL
        tip <- getTip
        if (hashPrevHeader == tip) then do
            -- SIMPLIFY IT!!!
            let batch = fromJust (computedBatch <|>
                                 (Just $ foldr' prependToBatch [] txs))
            filterMemPool txs
            writeBatchToUtxo (PutTip (headerHash b) : batch)
        else
            logError "Error during application of block to Undo DB: No TIP"
  where
    prependToBatch :: IdTxWitness -> [BatchOp ssc] -> [BatchOp ssc]
    prependToBatch (txId, (Tx{..}, _)) batch =
        let
            keys = zipWith TxIn (repeat txId) [0..]
            delIn = map DelTxIn txInputs
            putOut = map (uncurry AddTxOut) $ zip keys txOutputs in
        foldr' (:) (foldr' (:) batch putOut) delIn --how we could simplify it?

-- | Given number of blocks to rollback and some sidechain to adopt it
-- checks if it can be done prior to transaction validity. Returns a
-- list of topsorted transactions, head ~ deepest block on success.
txVerifyBlocks :: forall ssc m . MonadDB ssc m => AltChain ssc
               -> m (Either Text [[IdTxWitness]])
txVerifyBlocks newChain = do
    utxoDB <- getUtxoDB
    verifyRes <- runTxpLDHolderUV
                     (foldM verifyDo (Right []) newChainTxs)
                     (UV.createFromDB utxoDB)
    return $
        case verifyRes of
          Left msg     ->
              Left msg
          Right accTxs ->
              Right (map convertFrom' . reverse $ accTxs)
  where
    newChainTxs :: [(SlotId, [(WithHash Tx, TxWitness)])]
    newChainTxs =
        map (\b -> (b ^. blockSlot,
                    over (each._1) withHash (b ^. blockTxws))) $
        rights (NE.toList newChain)

    verifyDo  :: Either Text [[(WithHash Tx, TxWitness)]]
              -> (SlotId, [(WithHash Tx, TxWitness)])
              ->  TxpLDHolder ssc m (Either Text [[(WithHash Tx, TxWitness)]])
    verifyDo er@(Left _) _ = return er
    verifyDo (Right accTxs) (slotId, txws) = do
        res <- verifyAndApplyTxs txws
        return $
            case res of
              Left reason -> Left  (sformat eFormat slotId reason)
              Right txws' -> Right (txws':accTxs)
    eFormat =
        "Failed to apply transactions on block from slot " %
        slotIdF%", error: "%build

processTx :: MinTxpWorkMode ssc m => IdTxWitness -> m ProcessTxRes
processTx itw@(_, (tx, _)) = do
    tipBefore <- getTip
    resolved <-
      foldM (\s inp -> maybe s (\x -> HM.insert inp x s) <$> utxoGet inp)
            mempty (txInputs tx)
    db <- getUtxoDB
    modifyTxpLD (\txld@(_, mp, tip) ->
        let localSize = localTxsSize mp in
        if tipBefore == tip then
            if localSize < maxLocalTxs
                then processTxDo txld resolved db itw
                else (PTRoverwhelmed, txld)
        else
            (mkPTRinvalid ["Tips aren't same"], txld)
        )

processTxDo :: TxpLD ssc -> HM.HashMap TxIn TxOut -> DB ssc
            -> IdTxWitness -> (ProcessTxRes, TxpLD ssc)
processTxDo ld@(uv, mp, tip) resolvedIns utxoDB (id, (tx, txw))
    | HM.member id locTxs = (PTRknown, ld)
    | otherwise =
        case verifyRes of
            VerSuccess        -> newState addUtxo' delUtxo' locTxs locTxsSize
            VerFailure errors -> ((mkPTRinvalid errors), ld)
  where
    verifyRes = verifyTxPure inputResolver (tx, txw)
    locTxs = localTxs mp
    locTxsSize = localTxsSize mp
    addUtxo' = addUtxo uv
    delUtxo' = delUtxo uv
    inputResolver tin
        | HS.member tin delUtxo' = Nothing
        | otherwise =
            maybe (HM.lookup tin addUtxo') Just (HM.lookup tin resolvedIns)
    newState nAddUtxo nDelUtxo oldTxs oldSize =
        let keys = zipWith TxIn (repeat id) [0 ..]
            zipKeys = zip keys (txOutputs tx)
            newAddUtxo' = foldl' (flip $ uncurry HM.insert) nAddUtxo zipKeys
            newDelUtxo' = foldl' (flip HS.insert) nDelUtxo (txInputs tx)
        in ( PTRadded
           , ( UtxoView newAddUtxo' newDelUtxo' utxoDB
             , MemPool (HM.insert id (tx, txw) oldTxs) (oldSize + 1)
             , tip))

-- | Rollback last @n@ blocks. This will replace current utxo to utxo
-- of desired depth block and also filter local transactions so they
-- can be applied. @tx@ prefix is used, because rollback may happen in
-- other storages as well.
txRollbackBlocks :: (Ssc ssc, WithLogger m, MonadDB ssc m, MonadThrow m) => Word -> m ()
txRollbackBlocks (fromIntegral -> n) = replicateM_ n txRollbackBlock

-- | Rollback last block
txRollbackBlock :: (Ssc ssc, WithLogger m, MonadDB ssc m, MonadThrow m) => m ()
txRollbackBlock = getTip >>=
    (\tip ->
        aifM (getBlock tip) (\block ->
            aifM (getUndo tip) (\undo -> do
                let txs = getTxs block
                --TODO more detailed message must be here
                unless (length undo == length txs)
                    $ panic "Number of txs must be equal length of undo"
                let batchOrError = foldl' prependToBatch (Right []) $ zip txs undo
                case batchOrError of
                    Left msg    -> panic msg
                    Right batch -> writeBatchToUtxo $ PutTip (headerHash block) : batch
                    -- If we store block cache in UtxoView we must invalidate it
                )
            (errorMsg "No Undo for block")) -- should we use here panic, right?
            --(errorMsg $ sformat ("No Undo for block with hash: "%build) tip)
        (errorMsg "No Block"))
        --(errorMsg $ sformat ("No Block with hash: "%build) tip)
  where
    getTxs (Left _)   = []
    getTxs (Right mb) = map fst $ mb ^. blockTxws

    errorMsg msg = logError $ sformat ("Error during rollback block from Undo DB: "%text) msg

    prependToBatch :: Either Text [BatchOp ssc] -> (Tx, [TxOut]) -> Either Text [BatchOp ssc]
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
filterMemPool blockTxs = modifyTxpLD_ (\(uv, mp, tip) ->
    let newMPTxs = (localTxs mp) `HM.difference` (HM.fromList blockTxs) in
    (uv, MemPool newMPTxs (HM.size newMPTxs), tip))

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Removed from MemPool invalid transactions
normalizeTxpLD :: (MonadDB ssc m, MonadTxpLD ssc m, MonadThrow m)
               => m ()
normalizeTxpLD = do
    utxoTip <- getTip
    mpTxs <- HM.toList . localTxs <$> getMemPool
    emptyUtxoView <- UV.createFromDB <$> getUtxoDB
    let emptyMemPool = MemPool mempty 0
    maybe
        (setTxpLD (emptyUtxoView, emptyMemPool, utxoTip))
        (\topsorted -> do
             (validTxs, newUtxoView) -- we run this code in temporary TxpLDHolder
                  <-
                 runTxpLDHolderUV (findValid topsorted) emptyUtxoView
             setTxpLD (newState newUtxoView validTxs utxoTip))
        (topsortTxs (\(i, (t, _)) -> WithHash t i) mpTxs)
  where
    findValid topsorted = do
        validTxs' <- foldlM canApply [] topsorted
        newUtxoView' <- getUtxoView
        return (validTxs', newUtxoView')
    newState newUtxoView validTxs utxoTip =
        (newUtxoView, MemPool (HM.fromList validTxs) (length validTxs), utxoTip)
    canApply xs itw@(_, (tx, txw)) = do
        verifyRes <- verifyTxUtxo (tx, txw)
        case verifyRes of
            VerSuccess -> do
                applyTxToUtxo' itw
                return (itw : xs)
            VerFailure _ -> return xs

isGenesisBlock :: Block ssc -> Bool
isGenesisBlock (Left _) = True
isGenesisBlock _        = False
