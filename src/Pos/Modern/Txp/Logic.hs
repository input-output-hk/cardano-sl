{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Internal state of the transaction-handling worker. Trasnaction
-- processing logic.

module Pos.Modern.Txp.Logic
       (
         txVerifyBlocks
       , txApplyBlocks
       , processTx
       , txRollbackBlocks
       ) where

import           Control.Lens                    (each, over, (^.), _1)
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as NE
import           Formatting                      (sformat, stext, (%))
import           System.Wlog                     (WithLogger)
import           Universum

import           Pos.Constants                   (maxLocalTxs)
import           Pos.Crypto                      (WithHash (..), hash, withHash)
import           Pos.Modern.DB                   (DB, MonadDB, getUtxoDB)
import           Pos.Modern.DB.Utxo              (BatchOp (..), getTip, writeBatchToUtxo)
import           Pos.Modern.Txp.Class            (MonadTxpLD (..), TxpLD)
import           Pos.Modern.Txp.Error            (TxpError (..))
import           Pos.Modern.Txp.Holder           (TxpLDHolder, runTxpLDHolderUV)
import           Pos.Modern.Txp.Storage.Types    (MemPool (..), UtxoView (..))
import qualified Pos.Modern.Txp.Storage.UtxoView as UV
import           Pos.Ssc.Class.Types             (Ssc)
import           Pos.State.Storage.Types         (AltChain, ProcessTxRes (..),
                                                  mkPTRinvalid)
import           Pos.Types                       (Block, IdTxWitness, MonadUtxo,
                                                  MonadUtxoRead (utxoGet), SlotId,
                                                  Tx (..), TxId, TxIn (..), TxOut,
                                                  TxWitness, Undo, applyTxToUtxo',
                                                  blockSlot, blockTxs, blockTxws,
                                                  blockTxws, headerHash, prevBlockL,
                                                  slotIdF, topsortTxs, verifyTxPure)
import           Pos.Types.Utxo                  (verifyAndApplyTxs, verifyTxUtxo)
import           Pos.Util                        (inAssertMode, _neHead)

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

-- | Apply chain of /definitely/ valid blocks to state on transactions
-- processing.
txApplyBlocks :: TxpWorkMode ssc m => AltChain ssc -> m ()
txApplyBlocks blocks = do
    tip <- getTip
    when (tip /= blocks ^. _neHead . prevBlockL) $ throwM $
        TxpCantApplyBlocks "oldest block in AltChain is not based on tip"
    inAssertMode $
        do verdict <- txVerifyBlocks blocks
           case verdict of
               Right _ -> pass
               Left errors ->
                   panic $ "txVerifyBlocks failed: " <> errors
    -- Apply all the blocks' transactions
    -- TODO actually, we can improve it: we can use UtxoView from txVerifyBlocks
    -- Now we recalculate TxIn which must be removed from Utxo DB or added to Utxo DB
    -- I can improve it, if it is bottlneck
    -- We apply all blocks and filter mempool for every block
    mapM_ txApplyBlock $ NE.toList blocks
    normalizeTxpLD

txApplyBlock
    :: TxpWorkMode ssc m
    => Block ssc -> m ()
txApplyBlock (Left _) = pass
txApplyBlock (Right blk) = do
    let hashPrevHeader = blk ^. prevBlockL
    tip <- getTip
    when (tip /= hashPrevHeader) $
        panic
            "disaster, tip mismatch in txApplyBlock, probably semaphore doesn't work"
    let batch = foldr' prependToBatch [] txsAndIds
    filterMemPool txsAndIds
    writeBatchToUtxo (PutTip (headerHash blk) : batch)
  where
    txs = toList $ blk ^. blockTxs
    txsAndIds = map (\tx -> (hash tx, tx)) txs
    prependToBatch :: (TxId, Tx) -> [BatchOp ssc] -> [BatchOp ssc]
    prependToBatch (txId, Tx {..}) batch =
        let keys = zipWith TxIn (repeat txId) [0 ..]
            delIn = map DelTxIn txInputs
            putOut = map (uncurry AddTxOut) $ zip keys txOutputs
        in foldr' (:) (foldr' (:) batch putOut) delIn --how we could simplify it?

-- | Verify whether sequence of blocks can be applied to current Tx state.
txVerifyBlocks
    :: forall ssc m.
       MonadDB ssc m
    => AltChain ssc -> m (Either Text (NonEmpty Undo))
txVerifyBlocks newChain = do
    utxoDB <- getUtxoDB
    fmap (NE.fromList . reverse) <$>
      runTxpLDHolderUV
        (foldM verifyDo (Right []) newChainTxs)
        (UV.createFromDB utxoDB)
  where
    newChainTxs :: [(SlotId, [(WithHash Tx, TxWitness)])]
    newChainTxs =
        map (\b -> (b ^. blockSlot, over (each . _1) withHash (b ^. blockTxws))) $
        rights (NE.toList newChain)
    verifyDo
        :: Either Text [Undo]
        -> (SlotId, [(WithHash Tx, TxWitness)])
        -> TxpLDHolder ssc m (Either Text [Undo])
    verifyDo failure@(Left _) _ = pure failure
    verifyDo undos (slotId, txws) =
        attachSlotId slotId <$>
        (liftA2 (flip (:)) undos) <$>
        verifyAndApplyTxs txws
    attachSlotId _ suc@(Right _) = suc
    attachSlotId sId (Left errors) =
        Left $ (sformat ("[Block's slot = "%slotIdF % "]"%stext) sId) errors

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
            Right _     -> newState addUtxo' delUtxo' locTxs locTxsSize
            Left errors -> (PTRinvalid errors, ld)
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

-- | Head of list is the youngest block
txRollbackBlocks :: (WithLogger m, MonadDB ssc m)
                 => NonEmpty (Block ssc, Undo) -> m ()
txRollbackBlocks = mapM_ txRollbackBlock

-- | Rollback block
txRollbackBlock :: (WithLogger m, MonadDB ssc m)
                => (Block ssc, Undo) -> m ()
txRollbackBlock (block, undo) = do
    let txs = getTxs block
    --TODO more detailed message must be here
    unless (length undo == length txs)
        $ panic "Number of txs must be equal length of undo"
    let batchOrError = foldl' prependToBatch (Right []) $ zip txs undo
    case batchOrError of
        Left msg    -> panic msg
        Right batch -> writeBatchToUtxo $ PutTip (block ^. prevBlockL) : batch
        -- If we store block cache in UtxoView we must invalidate it
  where
    getTxs (Left _)   = []
    getTxs (Right mb) = map fst $ mb ^. blockTxws

    prependToBatch :: Either Text [BatchOp ssc] -> (Tx, [TxOut]) -> Either Text [BatchOp ssc]
    prependToBatch batchOrError (tx@Tx{..}, undoTx) = do
        batch <- batchOrError
        --TODO more detailed message must be here
        unless (length undoTx == length txInputs) $ Left "Number of txInputs must be equal length of undo"
        let txId = hash tx
            keys = zipWith TxIn (repeat txId) [0..]
            putIn = map (uncurry AddTxOut) $ zip txInputs undoTx
            delOut = map DelTxIn $ take (length txOutputs) keys
        return $ foldr' (:) (foldr' (:) batch putIn) delOut --how we could simplify it?

-- | Remove from mem pool transactions from block
filterMemPool :: MonadTxpLD ssc m => [(TxId, Tx)]  -> m ()
filterMemPool txs = modifyTxpLD_ (\(uv, mp, tip) ->
    let newMPTxs = (localTxs mp) `HM.difference` (HM.fromList txs) in
    (uv, MemPool newMPTxs (HM.size newMPTxs), tip))

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Removed from MemPool invalid transactions
normalizeTxpLD :: (MonadDB ssc m, MonadTxpLD ssc m)
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
            Right _ -> do
                applyTxToUtxo' itw
                return (itw : xs)
            Left _ -> return xs
