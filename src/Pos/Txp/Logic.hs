{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction processing logic.

module Pos.Txp.Logic
       (
         txVerifyBlocks
       , txApplyBlocks
       , processTx
       , txRollbackBlocks
       , normalizeTxpLD
       ) where

import           Control.Lens           (each)
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import qualified Data.List.NonEmpty     as NE
import           Formatting             (build, sformat, stext, (%))
import           System.Wlog            (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Constants          (maxLocalTxs)
import           Pos.Crypto             (WithHash (..), hash, withHash)
import           Pos.DB                 (DB, MonadDB,
                                         SomePrettyBatchOp (SomePrettyBatchOp), getUtxoDB)
import           Pos.DB.GState          (BalancesOp (..), CommonOp (..), UtxoOp (..),
                                         getTip, getTotalFtsStake)
import           Pos.Ssc.Class.Types    (Ssc)
import           Pos.Txp.Class          (MonadTxpLD (..), TxpLD, getUtxoView)
import           Pos.Txp.Error          (TxpError (..))
import           Pos.Txp.Holder         (TxpLDHolder, runLocalTxpLDHolder)
import           Pos.Txp.Types          (BalancesView (..), MemPool (..),
                                         MonadBalances (..), UtxoView (..))
import           Pos.Txp.Types.Types    (ProcessTxRes (..), mkPTRinvalid)
import qualified Pos.Txp.Types.UtxoView as UV
import           Pos.Types              (Block, Blund, Coin, MonadUtxo,
                                         MonadUtxoRead (utxoGet), NEBlocks, SlotId,
                                         StakeholderId, Tx (..), TxAux,
                                         TxDistribution (..), TxId, TxIn (..), TxOutAux,
                                         TxUndo, TxWitness, Undo, VTxGlobalContext (..),
                                         VTxLocalContext (..), applyTxToUtxo', blockSlot,
                                         blockTxas, coinToInteger, headerHash, mkCoin,
                                         prevBlockL, slotIdF, sumCoins, sumCoins,
                                         topsortTxs, txOutStake, undoTx, verifyTxPure)
import           Pos.Types.Coin         (unsafeAddCoin, unsafeIntegerToCoin,
                                         unsafeSubCoin)
import           Pos.Types.Utxo         (verifyAndApplyTxs, verifyTxUtxo)
import           Pos.Util               (inAssertMode, _neHead)

type TxpWorkMode ssc m = ( Ssc ssc
                         , WithLogger m
                         , MonadDB ssc m
                         , MonadTxpLD ssc m
                         , MonadUtxo m
                         , MonadThrow m)

type MinTxpWorkMode ssc m = ( MonadDB ssc m
                            , MonadTxpLD ssc m
                            , MonadUtxo m
                            , MonadThrow m
                            , WithLogger m)

-- | Apply chain of /definitely/ valid blocks to state on transactions
-- processing.
txApplyBlocks :: TxpWorkMode ssc m => NonEmpty (Blund ssc) -> m (NonEmpty SomePrettyBatchOp)
txApplyBlocks blunds = do
    let blocks = map fst blunds
    tip <- getTip
    when (tip /= blocks ^. _neHead . prevBlockL) $ throwM $
        TxpCantApplyBlocks "oldest block in NEBlocks is not based on tip"
    inAssertMode $
        do verdict <- txVerifyBlocks blocks
           case verdict of
               Right _ -> pass
               Left errors ->
                   panic $ "txVerifyBlocks failed: " <> errors
    -- Apply all the block's transactions
    -- TODO actually, we can improve it: we can use UtxoView from txVerifyBlocks
    -- Now we recalculate TxIn which must be removed from Utxo DB or added to Utxo DB

    -- We apply all blocks and filter mempool for every block
    total <- getTotalFtsStake
    db <- getUtxoDB
    evalStateT (mapM txApplyBlock blunds) (BalancesView mempty total db)

txApplyBlock
    :: ( Ssc ssc
       , WithLogger m,
         MonadBalances ssc m,
         MonadTxpLD ssc m)
    => Blund ssc -> m SomePrettyBatchOp
txApplyBlock (blk, undo) = do
    let batch = foldr' prependToBatch [] txsAndIds
    let putTip = SomePrettyBatchOp $ PutTip (headerHash blk)
    filterMemPool txsAndIds
    let (txOutPlus, txInMinus) = concatStakes (txas, undoTx undo)
    stakesBatch <- recomputeStakes txOutPlus txInMinus
    pure $ SomePrettyBatchOp $ [stakesBatch, SomePrettyBatchOp batch, putTip]
  where
    txas = either (const []) (toList . view blockTxas) blk
    txsAndIds = map (\tx -> (hash (tx ^. _1), (tx ^. _1, tx ^. _3))) txas
    prependToBatch :: (TxId, (Tx, TxDistribution))
                   -> [SomePrettyBatchOp] -> [SomePrettyBatchOp]
    prependToBatch (txId, (Tx{..}, distr)) batch =
        let keys = zipWith TxIn (repeat txId) [0 ..]
            delIn = map (SomePrettyBatchOp . DelTxIn) txInputs
            putOut = zipWith (\i o -> SomePrettyBatchOp $ AddTxOut i o)
                         keys
                         (zip txOutputs (getTxDistribution distr))
        in foldr' (:) (foldr' (:) batch putOut) delIn --how we could simplify it?

-- | Verify whether sequence of blocks can be applied to current Tx
-- state. This function doesn't make pure checks for transactions,
-- they are assumed to be done earlier.
txVerifyBlocks
    :: forall ssc m.
       MonadDB ssc m
    => NEBlocks ssc -> m (Either Text (NonEmpty TxUndo))
txVerifyBlocks newChain = do
    utxoDB <- getUtxoDB
    fmap (NE.fromList . reverse) <$>
      runLocalTxpLDHolder
        (foldM verifyDo (Right []) newChainTxs)
        (UV.createFromDB utxoDB)
  where
    -- Left for genesis blocks, Right for main blocks
    newChainTxs
        :: [Either () (SlotId, [(WithHash Tx, TxWitness, TxDistribution)])]
    newChainTxs = map f (NE.toList newChain)
      where
        f (Left _)  = Left ()
        f (Right b) = Right (b ^. blockSlot,
                             over (each . _1) withHash (b ^. blockTxas))
    verifyDo
        :: Either Text [TxUndo]
        -> Either () (SlotId, [(WithHash Tx, TxWitness, TxDistribution)])
        -> TxpLDHolder ssc m (Either Text [TxUndo])
    verifyDo (Left err) _ = pure (Left err)
    -- a genesis block doesn't need to be undone
    verifyDo (Right undos) (Left ()) = pure (Right ([]:undos))
    -- handling a main block
    verifyDo (Right undos) (Right (slotId, txas)) =
        verifyAndApplyTxs False txas <&> \case
            Right undo  -> Right (undo:undos)
            Left errors -> Left (attachSlotId slotId errors)
    attachSlotId sId errors =
        sformat ("[Block's slot = "%slotIdF % "]"%stext) sId errors

-- CHECK: @processTx
-- #processTxDo
processTx :: MinTxpWorkMode ssc m => (TxId, TxAux) -> m ProcessTxRes
processTx itw@(txId, (tx, _, _)) = do
    tipBefore <- getTip
    resolved <-
      foldM (\s inp -> maybe s (\x -> HM.insert inp x s) <$> utxoGet inp)
            mempty (txInputs tx)
    db <- getUtxoDB
    pRes <- modifyTxpLD $ \txld@(_, mp, _, tip) ->
        let localSize = localTxsSize mp in
        if tipBefore == tip
        then if localSize < maxLocalTxs
             then processTxDo txld resolved db itw
             else (PTRoverwhelmed, txld)
        else (mkPTRinvalid ["Tips aren't same"], txld)
    pRes <$ logDebug (sformat ("Transaction processed: "%build) txId)

-- CHECK: @processTxDo
-- #verifyTxPure
processTxDo :: TxpLD ssc -> HM.HashMap TxIn TxOutAux -> DB ssc
            -> (TxId, TxAux) -> (ProcessTxRes, TxpLD ssc)
processTxDo ld@(uv, mp, undos, tip) resolvedIns utxoDB (id, (tx, txw, txd))
    | HM.member id locTxs = (PTRknown, ld)
    | otherwise =
        case verifyRes of
            Right _     -> newState addUtxo' delUtxo' locTxs locTxsSize undos
            Left errors -> (PTRinvalid errors, ld)
  where
    verifyRes =
        verifyTxPure True VTxGlobalContext inputResolver (tx, txw, txd)
    locTxs = localTxs mp
    locTxsSize = localTxsSize mp
    addUtxo' = addUtxo uv
    delUtxo' = delUtxo uv
    inputResolver tin
        | HS.member tin delUtxo' = Nothing
        | otherwise =
            VTxLocalContext <$>
            maybe (HM.lookup tin addUtxo') Just (HM.lookup tin resolvedIns)
    prependToUndo undo inp =
        fromMaybe (panic "Input not resolved")
                  (HM.lookup inp resolvedIns) : undo
    newState nAddUtxo nDelUtxo oldTxs oldSize oldUndos =
        let keys = zipWith TxIn (repeat id) [0 ..]
            zipKeys = zip keys (txOutputs tx `zip` getTxDistribution txd)
            addUtxoMembers = zip (txInputs tx) $ map (`HM.member` nAddUtxo) (txInputs tx)
            squashedDels = map fst $ filter snd addUtxoMembers
            notSquashedDels = map fst $ filter (not . snd) addUtxoMembers
            newAddUtxo' = foldl' (flip $ uncurry HM.insert)
                                 (foldl' (flip HM.delete) nAddUtxo squashedDels)
                                 zipKeys
            newDelUtxo' = foldl' (flip HS.insert) nDelUtxo notSquashedDels
            newUndos = HM.insert id (reverse $ foldl' prependToUndo [] (txInputs tx)) oldUndos
        in ( PTRadded
           , ( UtxoView newAddUtxo' newDelUtxo' utxoDB
             , MemPool (HM.insert id (tx, txw, txd) oldTxs) (oldSize + 1)
             , newUndos
             , tip))

-- | Head of list is the youngest block
txRollbackBlocks :: (WithLogger m, MonadDB ssc m)
                 => NonEmpty (Block ssc, Undo) -> m (NonEmpty SomePrettyBatchOp)
txRollbackBlocks blunds = do
    total <- getTotalFtsStake
    db <- getUtxoDB
    evalStateT (mapM txRollbackBlock blunds) (BalancesView mempty total db)

-- | Rollback block
txRollbackBlock :: ( WithLogger m,
                     MonadBalances ssc m)
                => (Block ssc, Undo) -> m SomePrettyBatchOp
txRollbackBlock (block, undo) = do
    --TODO more detailed message must be here
    unless (length (undoTx undo) == length txs)
        $ panic "Number of txs must be equal length of undo"
    let batchOrError = foldl' prependToBatch (Right []) $ zip txs $ undoTx undo
    -- If we store block cache in UtxoView we must invalidate it
    -- Stakes/balances part
    let (txOutMinus, txInPlus) = concatStakes (txas, undoTx undo)
    let putTip = SomePrettyBatchOp $ PutTip (block ^. prevBlockL)
    stakesBatch <- recomputeStakes txInPlus txOutMinus
    either panic (pure . SomePrettyBatchOp . (\x->SomePrettyBatchOp x : [stakesBatch, putTip]))
           batchOrError
  where
    txas = either (const []) (toList . view blockTxas) block
    txs = either (const []) (toList . map (^. _1) . view blockTxas) block

    prependToBatch :: Either Text [SomePrettyBatchOp] ->
                      (Tx, [TxOutAux]) ->
                      Either Text [SomePrettyBatchOp]
    prependToBatch batchOrError (tx@Tx{..}, undoTx) = do
        batch <- batchOrError
        --TODO more detailed message must be here
        unless (length undoTx == length txInputs) $
            Left "Number of txInputs must be equal length of undo"
        let txId = hash tx
            keys = zipWith TxIn (repeat txId) [0..]
            putIn = zipWith (\i o -> SomePrettyBatchOp $ AddTxOut i o) txInputs undoTx
            delOut = map (SomePrettyBatchOp . DelTxIn) $ take (length txOutputs) keys
        return $ foldr' (:) (foldr' (:) batch putIn) delOut --how we could simplify it?

recomputeStakes :: (WithLogger m, MonadBalances ssc m)
                => [(StakeholderId, Coin)]
                -> [(StakeholderId, Coin)]
                -> m SomePrettyBatchOp
recomputeStakes plusDistr minusDistr = do
    resolvedStakes <- mapM (\ad ->
                              maybe (mkCoin 0 <$ logInfo (createInfo ad))
                                    pure =<< getStake ad)
                           needResolve
    totalStake <- getTotalStake
    let positiveDelta = sumCoins (map snd plusDistr)
    let negativeDelta = sumCoins (map snd minusDistr)
    let newTotalStake = unsafeIntegerToCoin $
                        coinToInteger totalStake + positiveDelta - negativeDelta

    let newStakes
          = HM.toList $
              calcNegStakes minusDistr
                  (calcPosStakes $ zip needResolve resolvedStakes ++ plusDistr)
    setTotalStake newTotalStake
    mapM_ (uncurry setStake) newStakes
    pure $
        SomePrettyBatchOp $
        (SomePrettyBatchOp $ PutFtsSum newTotalStake) :
        map (SomePrettyBatchOp . uncurry PutFtsStake) newStakes
  where
    createInfo = sformat ("Stake for " %build % " will be created in UtxoDB")
    needResolve =
        HS.toList $
        HS.fromList (map fst plusDistr) `HS.union`
        HS.fromList (map fst minusDistr)
    calcPosStakes distr = foldl' plusAt HM.empty distr
    calcNegStakes distr hm = foldl' minusAt hm distr
    -- @pva701 says it's not possible to get negative coin here. We *can* in
    -- theory get overflow because we're adding and only then subtracting,
    -- but in practice it won't happen unless someone has 2^63 coins or
    -- something.
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    minusAt hm (key, val) =
        HM.alter (maybe err (\v -> Just (unsafeSubCoin v val))) key hm
      where
        err = panic ("recomputeStakes: no stake for " <> show key)

concatStakes :: ([TxAux], TxUndo) -> ([(StakeholderId, Coin)]
                                     ,[(StakeholderId, Coin)])
concatStakes (txas, undo) = (txasTxOutDistr, undoTxInDistr)
  where
    txasTxOutDistr = concatMap concatDistr txas
    undoTxInDistr = concatMap txOutStake (concat undo)
    concatDistr (Tx{..}, _, distr)
        = concatMap txOutStake (zip txOutputs (getTxDistribution distr))

-- | Remove from mem pool transactions from block
filterMemPool :: MonadTxpLD ssc m => [(TxId, (Tx, TxDistribution))] -> m ()
filterMemPool txs = modifyTxpLD_ (\(uv, mp, undos, tip) ->
    let blkTxs = HM.fromList txs
        newMPTxs = (localTxs mp) `HM.difference` blkTxs
        newUndos = undos `HM.difference` blkTxs in
    (uv, MemPool newMPTxs (HM.size newMPTxs), newUndos, tip))

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
normalizeTxpLD :: (MonadDB ssc m, MonadTxpLD ssc m)
               => m ()
normalizeTxpLD = do
    utxoTip <- getTip
    (_, memPool, undos, _) <- getTxpLD
    let mpTxs = HM.toList . localTxs $ memPool
    emptyUtxoView <- UV.createFromDB <$> getUtxoDB
    let emptyMemPool = MemPool mempty 0
    maybe
        (setTxpLD (emptyUtxoView, emptyMemPool, mempty, utxoTip))
        (\topsorted -> do
             -- we run this code in temporary TxpLDHolder
             (validTxs, newUtxoView) <-
                 runLocalTxpLDHolder (findValid topsorted) emptyUtxoView
             setTxpLD $ newState newUtxoView validTxs undos utxoTip)
        (topsortTxs (\(i, (t, _, _)) -> WithHash t i) mpTxs)
  where
    findValid topsorted = do
        validTxs' <- foldlM canApply [] topsorted
        newUtxoView' <- getUtxoView
        return (validTxs', newUtxoView')
    newState newUtxoView validTxs undos utxoTip =
        let newTxs = HM.fromList validTxs in
        (newUtxoView, MemPool newTxs (length validTxs), undos, utxoTip)
    canApply xs itxa@(_, txa) = do
        -- Pure checks are not done here, because they are done
        -- earlier, when we accept transaction.
        verifyRes <- verifyTxUtxo False txa
        case verifyRes of
            Right _ -> do
                applyTxToUtxo' itxa
                return (itxa : xs)
            Left _ -> return xs
