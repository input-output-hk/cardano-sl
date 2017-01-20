{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic of blocks processing.

module Pos.Block.Logic
       (
         -- * Common/Utils
         lcaWithMainChain
       , tipMismatchMsg
       , withBlkSemaphore
       , withBlkSemaphore_

         -- * Headers
       , ClassifyHeaderRes (..)
       , classifyNewHeader
       , ClassifyHeadersRes (..)
       , classifyHeaders
       , getHeadersFromManyTo
       , getHeadersOlderExp
       , getHeadersFromToIncl

         -- * Blocks
       , applyBlocks
       , applyWithRollback
       , rollbackBlocks
       , verifyAndApplyBlocks
       , createGenesisBlock
       , createMainBlock
       ) where

import           Control.Lens              (_Wrapped)
import           Control.Monad.Catch       (try)
import           Control.Monad.Except      (ExceptT (ExceptT), runExceptT, throwError,
                                            withExceptT)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Default              (Default (def))
import qualified Data.HashMap.Strict       as HM
import           Data.List.NonEmpty        ((<|))
import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as T
import           Formatting                (build, int, ords, sformat, stext, (%))
import           Serokell.Util.Text        (listJson)
import           Serokell.Util.Verify      (VerificationRes (..), formatAllErrors,
                                            isVerSuccess, verResToMonadError)
import           System.Wlog               (CanLog, HasLoggerName, logDebug, logError,
                                            logInfo)
import           Universum

import           Pos.Block.Logic.Internal  (applyBlocksUnsafe, rollbackBlocksUnsafe,
                                            withBlkSemaphore, withBlkSemaphore_)
import           Pos.Block.Types           (Blund, Undo (..))
import           Pos.Constants             (blkSecurityParam, curProtocolVersion,
                                            curSoftwareVersion, epochSlots,
                                            recoveryHeadersMessage, slotSecurityParam)
import           Pos.Context               (NodeContext (ncSecretKey), getNodeContext,
                                            lrcActionOnEpochReason)
import           Pos.Crypto                (SecretKey, WithHash (WithHash), hash,
                                            shortHashF)
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.DB                    (DBError (..), MonadDB)
import qualified Pos.DB                    as DB
import qualified Pos.DB.GState             as GS
import qualified Pos.DB.Lrc                as LrcDB
import           Pos.Delegation.Logic      (delegationVerifyBlocks, getProxyMempool)
import           Pos.Lrc.Error             (LrcError (..))
import           Pos.Lrc.Worker            (lrcSingleShotNoLock)
import           Pos.Slotting              (getCurrentSlot)
import           Pos.Ssc.Class             (Ssc (..), SscWorkersClass (..))
import           Pos.Ssc.Extra             (sscGetLocalPayload, sscVerifyBlocks)
import           Pos.Txp.Class             (getLocalTxsNUndo)
import           Pos.Txp.Logic             (txVerifyBlocks)
import           Pos.Types                 (Block, BlockHeader, EpochIndex,
                                            EpochOrSlot (..), GenesisBlock, HeaderHash,
                                            MainBlock, MainExtraBodyData (..),
                                            MainExtraHeaderData (..), ProxySKEither,
                                            ProxySKSimple, SlotId (..), SlotLeaders,
                                            TxAux, TxId, VerifyHeaderParams (..),
                                            blockHeader, difficultyL, epochIndexL,
                                            epochOrSlot, flattenEpochOrSlot, genesisHash,
                                            getEpochOrSlot, headerHash, headerHashG,
                                            headerSlot, mkGenesisBlock, mkMainBlock,
                                            mkMainBody, prevBlockL, topsortTxs,
                                            verifyHeader, verifyHeaders,
                                            vhpVerifyConsensus)
import qualified Pos.Types                 as Types
import           Pos.Update.Core           (UpdatePayload (..))
import           Pos.Update.Logic          (usVerifyBlocks)
import           Pos.Update.Poll           (PollModifier, PollVerFailure)
import           Pos.Util                  (NE, NewestFirst (..), OldestFirst (..),
                                            inAssertMode, neZipWith3, spanSafe,
                                            toNewestFirst, toOldestFirst, _neHead,
                                            _neLast)
import           Pos.WorkMode              (WorkMode)

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

-- | Common error message
tipMismatchMsg :: Text -> HeaderHash -> HeaderHash -> Text
tipMismatchMsg action storedTip attemptedTip =
    sformat
        ("Can't "%stext%" block because of tip mismatch (stored is "
         %shortHashF%", attempted is "%shortHashF%")")
        action storedTip attemptedTip


-- | Find lca headers and main chain, including oldest header's parent hash.
lcaWithMainChain
    :: (WorkMode ssc m)
    => NewestFirst NE (BlockHeader ssc) -> m (Maybe HeaderHash)
lcaWithMainChain headers =
    fmap fst . find snd <$>
        mapM (\hh -> (hh,) <$> GS.isBlockInMainChain hh)
             -- take hash of parent of last BlockHeader and convert all headers to hashes
             (map headerHash (toList headers) ++
              [headers ^. _Wrapped . _neLast . prevBlockL])

----------------------------------------------------------------------------
-- Headers
----------------------------------------------------------------------------

-- | Result of single (new) header classification.
data ClassifyHeaderRes
    = CHContinues      -- ^ Header continues our main chain.
    | CHAlternative    -- ^ Header continues alternative chain which
                       -- is more difficult.
    | CHUseless !Text  -- ^ Header is useless.
    | CHInvalid !Text  -- ^ Header is invalid.

-- | Make `ClassifyHeaderRes` from list of error messages using
-- `CHRinvalid` constructor. Intended to be used with `VerificationRes`.
-- Note: this version forces computation of all error messages. It can be
-- made more efficient but less informative by using head, for example.
mkCHRinvalid :: [Text] -> ClassifyHeaderRes
mkCHRinvalid = CHInvalid . T.intercalate "; "

-- | Classify new header announced by some node. Result is represented
-- as ClassifyHeaderRes type.
classifyNewHeader
    :: (WorkMode ssc m)
    => BlockHeader ssc -> m ClassifyHeaderRes
-- Genesis headers seem useless, we can create them by ourselves.
classifyNewHeader (Left _) = pure $ CHUseless "genesis header is useless"
classifyNewHeader (Right header) = do
    curSlot <- getCurrentSlot
    -- First of all we check whether header is from current slot and
    -- ignore it if it's not.
    if curSlot == header ^. headerSlot
        then classifyNewHeaderDo <$> GS.getTip <*> DB.getTipBlock
        else pure $ CHUseless $ sformat
                 ("header is not for current slot: our is "%build%", header's is "%build)
                 curSlot (header ^. headerSlot)
  where
    classifyNewHeaderDo tip tipBlock
        -- If header's parent is our tip, we verify it against tip's header.
        | tip == header ^. prevBlockL =
            let vhp =
                    def
                    { vhpVerifyConsensus = True
                    , vhpPrevHeader = Just $ tipBlock ^. blockHeader
                    }
                verRes = verifyHeader vhp (Right header)
            in case verRes of
                   VerSuccess        -> CHContinues
                   VerFailure errors -> mkCHRinvalid errors
        -- If header's parent is not our tip, we check whether it's
        -- more difficult than our main chain.
        | tipBlock ^. difficultyL < header ^. difficultyL = CHAlternative
        -- If header can't continue main chain and is not more
        -- difficult than main chain, it's useless.
        | otherwise =
            CHUseless $
            "header doesn't continue main chain and is not more difficult"


-- | Result of multiple headers classification.
data ClassifyHeadersRes ssc
    = CHsValid (BlockHeader ssc) -- ^ Header list can be applied, LCA child attached.
    | CHsUseless !Text           -- ^ Header is useless.
    | CHsInvalid !Text           -- ^ Header is invalid.

-- | Classify headers received in response to 'GetHeaders' message.
--
-- * If there are any errors in chain of headers, CHsInvalid is returned.
-- * If chain of headers is a valid continuation or alternative branch,
-- lca child is returned.
-- * If chain of headers forks from our main chain too much, CHsUseless
-- is returned, because paper suggests doing so.
classifyHeaders
    :: WorkMode ssc m
    => NewestFirst NE (BlockHeader ssc) -> m (ClassifyHeadersRes ssc)
classifyHeaders headers = do
    let newestHash = headers ^. _Wrapped . _neHead . headerHashG
        oldestHash = headers ^. _Wrapped . _neLast . headerHashG
    tip <- GS.getTip
    haveOldest <- isJust <$> DB.getBlockHeader oldestHash
    let headersValid = isVerSuccess $ verifyHeaders True (headers & _Wrapped %~ toList)
    if | not headersValid ->
             pure $ CHsInvalid "Header chain is invalid"
       | not haveOldest ->
             pure $ CHsInvalid "Last block of the passed chain wasn't found locally"
       | newestHash == headerHash tip ->
             pure $ CHsUseless "Newest hash is the same as our tip"
       | otherwise -> fromMaybe uselessGeneral <$> processClassify
  where
    uselessGeneral =
        CHsUseless "Couldn't find lca -- maybe db state updated in the process"
    processClassify = runMaybeT $ do
        tipHeader <- view blockHeader <$> lift DB.getTipBlock
        lift $ logDebug $
            sformat ("Classifying headers: "%listJson) $ map (view headerHashG) headers
        lcaHash <- MaybeT $ lcaWithMainChain headers
        lca <- MaybeT $ DB.getBlockHeader lcaHash
        let depthDiff = tipHeader ^. difficultyL - lca ^. difficultyL
        lcaChild <- MaybeT $ pure $
            find (\bh -> bh ^. prevBlockL == headerHash lca) headers
        pure $ if
            | hash lca == hash tipHeader -> CHsValid lcaChild
            | depthDiff < 0 -> panic "classifyHeaders@depthDiff is negative"
            | depthDiff > blkSecurityParam ->
                  CHsUseless $
                  sformat ("Difficulty difference of (tip,lca) is "%int%
                           " which is more than blkSecurityParam = "%int)
                          depthDiff (blkSecurityParam :: Int)
            | otherwise -> CHsValid lcaChild

-- | Given a set of checkpoints @c@ to stop at and a terminating
-- header hash @h@, we take @h@ block (or tip if latter is @Nothing@)
-- and fetch the blocks until one of checkpoints is encountered. In
-- case we got deeper than 'recoveryHeadersMessage', we return
-- 'recoveryHeadersMessage' headers starting from the the newest
-- checkpoint that's in our main chain to the newest ones.
getHeadersFromManyTo
    :: forall ssc m. (MonadDB ssc m, Ssc ssc, CanLog m, HasLoggerName m)
    => NonEmpty HeaderHash  -- ^ Checkpoints; not guaranteed to be
                            --   in any particular order
    -> Maybe HeaderHash
    -> m (Maybe (NewestFirst NE (BlockHeader ssc)))
getHeadersFromManyTo checkpoints startM = runMaybeT $ do
    lift $ logDebug $
        sformat ("getHeadersFromManyTo: "%listJson%", start: "%build)
                checkpoints startM
    validCheckpoints <- MaybeT $
        nonEmpty . catMaybes <$>
        mapM DB.getBlockHeader (toList checkpoints)
    tip <- lift GS.getTip
    guard $ all ((/= tip) . headerHash) validCheckpoints
    let startFrom = fromMaybe tip startM
        parentIsCheckpoint bh =
            any (\c -> bh ^. prevBlockL == c ^. headerHashG) validCheckpoints
        whileCond bh = not (parentIsCheckpoint bh)
    headers <- MaybeT . fmap (_Wrapped nonEmpty) $
        DB.loadHeadersByDepthWhile whileCond recoveryHeadersMessage startFrom
    if parentIsCheckpoint $ headers ^. _Wrapped . _neHead
    then pure headers
    else do
        lift $ logDebug $ "getHeadersFromManyTo: giving headers in recovery mode"
        inMainCheckpoints <-
            MaybeT $ nonEmpty <$>
            filterM (GS.isBlockInMainChain . headerHash)
                    (NE.toList validCheckpoints)
        let lowestCheckpoint =
                maximumBy (comparing flattenEpochOrSlot) inMainCheckpoints
            loadUpCond _ h = h < recoveryHeadersMessage
        up <- lift $ GS.loadHeadersUpWhile lowestCheckpoint loadUpCond
        MaybeT $ pure $ _Wrapped nonEmpty (toNewestFirst up)

-- | Given a starting point hash (we take tip if it's not in storage)
-- it returns not more than 'blkSecurityParam' blocks distributed
-- exponentially base 2 relatively to the depth in the blockchain.
getHeadersOlderExp
    :: (MonadDB ssc m, Ssc ssc)
    => Maybe HeaderHash -> m (OldestFirst [] HeaderHash)
getHeadersOlderExp upto = do
    tip <- GS.getTip
    let upToReal = fromMaybe tip upto
    -- Using 'blkSecurityParam + 1' because fork can happen on k+1th one.
    allHeaders <-
        toOldestFirst <$> DB.loadHeadersByDepth (blkSecurityParam + 1) upToReal
    pure $ OldestFirst $
        selectIndices
            (getOldestFirst (map headerHash allHeaders))
            (twoPowers $ length allHeaders)
  where
    -- Powers of 2
    twoPowers n
        | n < 0 = panic $ "getHeadersOlderExp#twoPowers called w/" <> show n
    twoPowers 0 = []
    twoPowers 1 = [0]
    twoPowers n = (takeWhile (< (n - 1)) $ 0 : 1 : iterate (* 2) 2) ++ [n - 1]
    -- Effectively do @!i@ for any @i@ from the index list applied to
    -- source list. Index list should be increasing.
    selectIndices :: [a] -> [Int] -> [a]
    selectIndices elems ixs =
        let selGo _ [] _ = []
            selGo [] _ _ = []
            selGo ee@(e:es) ii@(i:is) skipped
                | skipped == i = e : selGo ee is skipped
                | otherwise = selGo es ii $ succ skipped
        in selGo elems ixs 0

-- CSL-396 don't load all the blocks into memory at once
-- | Given @from@ and @to@ headers where @from@ is older (not strict)
-- than @to@, and valid chain in between can be found, headers in
-- range @[from..to]@ will be found.
getHeadersFromToIncl
    :: forall ssc m .
       (MonadDB ssc m, Ssc ssc)
    => HeaderHash -> HeaderHash -> m (Maybe (OldestFirst NE HeaderHash))
getHeadersFromToIncl older newer = runMaybeT . fmap OldestFirst $ do
    -- oldest and newest blocks do exist
    start <- MaybeT $ DB.getBlockHeader newer
    end   <- MaybeT $ DB.getBlockHeader older
    guard $ flattenEpochOrSlot start >= flattenEpochOrSlot end
    let lowerBound = flattenEpochOrSlot end
    if newer == older
    then pure $ one newer
    else loadHeadersDo lowerBound (one newer) $ start ^. prevBlockL
  where
    loadHeadersDo
        :: Word64
        -> NonEmpty HeaderHash
        -> HeaderHash
        -> MaybeT m (NonEmpty HeaderHash)
    loadHeadersDo lowerBound hashes nextHash
        | nextHash == genesisHash = mzero
        | nextHash == older = pure $ nextHash <| hashes
        | otherwise = do
            nextHeader <- MaybeT $ DB.getBlockHeader nextHash
            guard $ flattenEpochOrSlot nextHeader > lowerBound
            -- hashes are being prepended so the oldest hash will be the last
            -- one to be prepended and thus the order is OldestFirst
            loadHeadersDo lowerBound (nextHash <| hashes) (nextHeader ^. prevBlockL)


----------------------------------------------------------------------------
-- Blocks verify/apply/rollback
----------------------------------------------------------------------------

-- -- CHECK: @verifyBlocksLogic
-- -- #txVerifyBlocks
-- -- #sscVerifyBlocks
-- -- #delegationVerifyBlocks
-- -- #usVerifyBlocks
-- | Verify new blocks. If parent of the first block is not our tip,
-- verification fails. This function checks everything from block, including
-- header, transactions, delegation data, SSC data.
verifyBlocksPrefix
    :: WorkMode ssc m
    => OldestFirst NE (Block ssc)
    -> m (Either Text (OldestFirst NE Undo, PollModifier))
verifyBlocksPrefix blocks = runExceptT $ do
    curSlot <- getCurrentSlot
    tipBlk <- DB.getTipBlock
    when (headerHash tipBlk /= blocks ^. _Wrapped . _neHead . prevBlockL) $
        throwError "the first block isn't based on the tip"
    verResToMonadError formatAllErrors $
        Types.verifyBlocks (Just curSlot) (over _Wrapped (tipBlk <|) blocks)
    verResToMonadError formatAllErrors =<< sscVerifyBlocks False blocks
    txUndo <- ExceptT $ txVerifyBlocks blocks
    pskUndo <- ExceptT $ delegationVerifyBlocks blocks
    (pModifier, usUndos) <- withExceptT pretty $ usVerifyBlocks blocks
    when (length txUndo /= length pskUndo) $
        throwError "Internal error of verifyBlocksPrefix: lengths of undos don't match"
    pure ( OldestFirst $ neZipWith3 Undo
               (getOldestFirst txUndo)
               (getOldestFirst pskUndo)
               (getOldestFirst usUndos)
         , pModifier)

-- | Applies blocks if they're valid. Takes one boolean flag
-- "rollback". Returns header hash of last applied block (new tip) on
-- success. Failure behaviour depends on "rollback" flag. If it's on,
-- all blocks applied inside this function will be rollbacked, so it
-- will do effectively nothing and return 'Left error'. If it's off,
-- it will try to apply as much blocks as it's possible and return
-- header hash of new tip. It's up to caller to log warning that
-- partial application happened.
verifyAndApplyBlocks
    :: (WorkMode ssc m, SscWorkersClass ssc)
    => Bool -> OldestFirst NE (Block ssc) -> m (Either Text HeaderHash)
verifyAndApplyBlocks rollback = verifyAndApplyBlocksInternal True rollback

-- See the description for verifyAndApplyBlocks. This method also
-- parameterizes LRC calculation which can be turned on/off with the first
-- flag.
verifyAndApplyBlocksInternal
    :: forall ssc m. (WorkMode ssc m, SscWorkersClass ssc)
    => Bool -> Bool -> OldestFirst NE (Block ssc) -> m (Either Text HeaderHash)
verifyAndApplyBlocksInternal lrc rollback blocks = runExceptT $ do
    tip <- GS.getTip
    let assumedTip = blocks ^. _Wrapped . _neHead . prevBlockL
    when (tip /= assumedTip) $ throwError $
        tipMismatchMsg "verify and apply" tip assumedTip
    rollingVerifyAndApply [] (spanEpoch blocks)
  where
    spanEpoch = over _1 OldestFirst . over _2 OldestFirst .  -- wrap both results
                spanSafe ((==) `on` view epochIndexL) .      -- do work
                getOldestFirst                               -- unwrap argument
    -- Applies as much blocks from failed prefix as possible. Argument
    -- indicates if at least some progress was done so we should
    -- return tip. Fail otherwise.
    applyAMAP e (OldestFirst []) True                   = throwError e
    applyAMAP _ (OldestFirst []) False                  = GS.getTip
    applyAMAP e (OldestFirst (block:xs)) nothingApplied = do
        lift (verifyBlocksPrefix (one block)) >>= \case
            Left e' -> applyAMAP e' (OldestFirst []) nothingApplied
            Right (OldestFirst (undo :| []), pModifier) -> do
                lift $ applyBlocksUnsafe (one (block, undo)) pModifier
                applyAMAP e (OldestFirst xs) False
            Right _ -> panic "verifyAndApplyBlocksInternal: applyAMAP: \
                             \verification of one block produced more than one undo"
    -- Rollbacks and returns an error
    failWithRollback e [] = throwError e
    failWithRollback e toRollback = do
        lift $ mapM_ rollbackBlocks toRollback
        throwError e
    -- TODO: write what this does
    -- TODO: also the first list is probably chronologically ordered as well?
    --       I don't know
    rollingVerifyAndApply
        :: [NewestFirst NE (Blund ssc)]
        -> (OldestFirst NE (Block ssc), OldestFirst [] (Block ssc))
        -> ExceptT Text m HeaderHash
    rollingVerifyAndApply blunds (prefix, suffix) = do
        lift (verifyBlocksPrefix prefix) >>= \case
            Left failure
                | rollback  -> failWithRollback failure blunds
                | otherwise -> applyAMAP failure
                                   (over _Wrapped toList prefix)
                                   (null blunds)
            Right (undos, pModifier) -> do
                let newBlunds = OldestFirst $ getOldestFirst prefix `NE.zip`
                                              getOldestFirst undos
                lift $ applyBlocksUnsafe newBlunds pModifier
                case getOldestFirst suffix of
                    []           -> GS.getTip
                    (genesis:xs) -> do
                        when lrc $ lift $
                            lrcSingleShotNoLock (genesis ^. epochIndexL)
                        rollingVerifyAndApply (toNewestFirst newBlunds : blunds) $
                            spanEpoch (OldestFirst (genesis:|xs))

-- | Apply definitely valid sequence of blocks. At this point we must
-- have verified all predicates regarding block (including txs and ssc
-- data checks). We also must have taken lock on block application
-- and ensured that chain is based on our tip. Blocks will be applied
-- per-epoch, calculating lrc when needed if flag is set.
applyBlocks
    :: forall ssc m . (WorkMode ssc m, SscWorkersClass ssc)
    => Bool -> PollModifier -> OldestFirst NE (Blund ssc) -> m ()
applyBlocks calculateLrc pModifier blunds = do
    applyBlocksUnsafe prefix pModifier
    case getOldestFirst suffix of
        [] -> pass
        (genesis:xs) -> do
            when calculateLrc $
                lrcSingleShotNoLock (genesis ^. _1 . epochIndexL)
            applyBlocks calculateLrc pModifier (OldestFirst (genesis:|xs))
  where
    (prefix, suffix) =
        over _1 OldestFirst . over _2 OldestFirst $
        spanSafe ((==) `on` view (_1 . epochIndexL)) (getOldestFirst blunds)

-- | Rollbacks blocks. Head must be the current tip.
rollbackBlocks
    :: (WorkMode ssc m)
    => NewestFirst NE (Blund ssc) -> m (Maybe Text)
rollbackBlocks blunds = do
    tip <- GS.getTip
    let firstToRollback = blunds ^. _Wrapped . _neHead . _1 . headerHashG
    if tip /= firstToRollback
    then pure $ Just $ tipMismatchMsg "rollback" tip firstToRollback
    else rollbackBlocksUnsafe blunds $> Nothing

-- | Rollbacks some blocks and then applies some blocks.
applyWithRollback
    :: (WorkMode ssc m, SscWorkersClass ssc)
    => NewestFirst NE (Blund ssc)  -- ^ Blocks to rollbck
    -> OldestFirst NE (Block ssc)  -- ^ Blocks to apply
    -> m (Either Text HeaderHash)
applyWithRollback toRollback toApply = runExceptT $ do
    tip <- GS.getTip
    when (tip /= newestToRollback) $ do
        throwError (tipMismatchMsg "rollback in 'apply with rollback'" tip newestToRollback)
    lift $ rollbackBlocksUnsafe toRollback
    tipAfterRollback <- GS.getTip
    when (tipAfterRollback /= expectedTipApply) $ do
        applyBack
        throwError (tipMismatchMsg "apply in 'apply with rollback'" tip newestToRollback)
    lift (verifyAndApplyBlocks True toApply) >>= \case
        -- We didn't succeed to apply blocks, so will apply
        -- rollbacked back.
        Left err -> do
            applyBack
            throwError err
        Right tipHash  -> pure tipHash
  where
    reApply = toOldestFirst toRollback
    applyBackFail (_ :: PollVerFailure) =
        throwM $ DBMalformed "applyWithRollback: can't verify just rollbacked blocks"
    applyBack = do
        verRes <- lift $ runExceptT $ usVerifyBlocks $ map fst reApply
        pModifier <- either applyBackFail (pure . fst) verRes
        lift $ applyBlocks True pModifier reApply
    expectedTipApply = toApply ^. _Wrapped . _neHead . prevBlockL
    newestToRollback = toRollback ^. _Wrapped . _neHead . _1 . headerHashG


----------------------------------------------------------------------------
-- GenesisBlock creation
----------------------------------------------------------------------------

-- | Create genesis block if necessary.
--
-- We create genesis block for current epoch when head of currently
-- known best chain is MainBlock corresponding to one of last
-- `slotSecurityParam` slots of (i - 1)-th epoch. Main check is that
-- epoch is `(last stored epoch + 1)`, but we also don't want to
-- create genesis block on top of blocks from previous epoch which are
-- not from last slotSecurityParam slots, because it's practically
-- impossible for them to be valid.
-- [CSL-481] We can do consider doing it though.
createGenesisBlock
    :: forall ssc m.
       WorkMode ssc m
    => EpochIndex -> m (Maybe (GenesisBlock ssc))
createGenesisBlock epoch = do
    leadersOrErr <-
        try $
        lrcActionOnEpochReason epoch "there are no leaders" LrcDB.getLeaders
    case leadersOrErr of
        Left UnknownBlocksForLrc ->
            Nothing <$ logInfo "createGenesisBlock: not enough blocks for LRC"
        Left err -> throwM err
        Right leaders -> withBlkSemaphore (createGenesisBlockDo epoch leaders)

shouldCreateGenesisBlock :: EpochIndex -> EpochOrSlot -> Bool
-- Genesis block for 0-th epoch is hardcoded.
shouldCreateGenesisBlock 0 _ = False
shouldCreateGenesisBlock epoch headEpochOrSlot =
    doCheck $ epochOrSlot (`SlotId` 0) identity headEpochOrSlot
  where
    doCheck SlotId {..} =
        siEpoch == epoch - 1 && siSlot >= epochSlots - slotSecurityParam

createGenesisBlockDo
    :: forall ssc m.
       WorkMode ssc m
    => EpochIndex
    -> SlotLeaders
    -> HeaderHash
    -> m (Maybe (GenesisBlock ssc), HeaderHash)
createGenesisBlockDo epoch leaders tip = do
    let noHeaderMsg =
            "There is no header is DB corresponding to tip from semaphore"
    tipHeader <-
        maybe (throwM $ DBMalformed noHeaderMsg) pure =<< DB.getBlockHeader tip
    logDebug $ sformat msgTryingFmt epoch tipHeader
    createGenesisBlockFinally tipHeader
  where
    emptyUndo = Undo [] [] def
    createGenesisBlockFinally tipHeader
        | shouldCreateGenesisBlock epoch (getEpochOrSlot tipHeader) = do
            let blk = mkGenesisBlock (Just tipHeader) epoch leaders
            let newTip = headerHash blk
            applyBlocksUnsafe (one (Left blk, emptyUndo)) def $>
                (Just blk, newTip)
        | otherwise = (Nothing, tip) <$ logShouldNot
    logShouldNot =
        logDebug
            "After we took lock for genesis block creation, we noticed that we shouldn't create it"
    msgTryingFmt =
        "We are trying to create genesis block for " %ords %
        " epoch, our tip header is\n" %build

----------------------------------------------------------------------------
-- MainBlock creation
----------------------------------------------------------------------------

-- | Create a new main block on top of best chain if possible.
-- Block can be created if:
-- • we know genesis block for epoch from given SlotId
-- • last known block is not more than 'slotSecurityParam' blocks away from
-- given SlotId
createMainBlock
    :: forall ssc m.
       WorkMode ssc m
    => SlotId
    -> Maybe ProxySKEither
    -> m (Either Text (MainBlock ssc))
createMainBlock sId pSk = withBlkSemaphore createMainBlockDo
  where
    msgFmt = "We are trying to create main block, our tip header is\n"%build
    createMainBlockDo tip = do
        tipHeader <- DB.getTipBlockHeader
        logInfo $ sformat msgFmt tipHeader
        case canCreateBlock sId tipHeader of
            Nothing  -> convertRes tip <$>
                runExceptT (createMainBlockFinish sId pSk tipHeader)
            Just err -> return (Left err, tip)
    convertRes oldTip (Left e) = (Left e, oldTip)
    convertRes _ (Right blk)   = (Right blk, headerHash blk)

canCreateBlock :: SlotId -> BlockHeader ssc -> Maybe Text
canCreateBlock sId tipHeader
    | sId > maxSlotId = Just "slot id is too big, we don't know recent block"
    | (EpochOrSlot $ Right sId) < headSlot =
        Just "slot id is not bigger than one from last known block"
    | otherwise = Nothing
  where
    headSlot = getEpochOrSlot tipHeader
    addSafe si =
        si {siSlot = min (epochSlots - 1) (siSlot si + slotSecurityParam)}
    maxSlotId = addSafe $ epochOrSlot (`SlotId` 0) identity headSlot

-- Here we assume that blkSemaphore has been taken.
createMainBlockFinish
    :: forall ssc m.
       WorkMode ssc m
    => SlotId
    -> Maybe ProxySKEither
    -> BlockHeader ssc
    -> ExceptT Text m (MainBlock ssc)
createMainBlockFinish slotId pSk prevHeader = do
    (localTxs, txUndo) <- getLocalTxsNUndo @ssc
    sscData <- maybe onNoSsc pure =<< sscGetLocalPayload @ssc slotId
    (localPSKs, pskUndo) <- lift getProxyMempool
    let convertTx (txId, (tx, _, _)) = WithHash tx txId
    sortedTxs <- maybe onBrokenTopo pure $ topsortTxs convertTx localTxs
    sk <- ncSecretKey <$> getNodeContext
    let blk = createMainBlockPure prevHeader sortedTxs pSk slotId localPSKs sscData sk
    let prependToUndo undos tx =
            fromMaybe (panic "Undo for tx not found")
                      (HM.lookup (fst tx) txUndo) : undos
    lift $ inAssertMode $ verifyBlocksPrefix (one (Right blk)) >>= \case
        Left err -> logError $ sformat ("We've created bad block: "%stext) err
        Right _ -> pass
    (pModifier,verUndo) <- runExceptT (usVerifyBlocks (one (Right blk))) >>= \case
        Left _ -> throwError "Couldn't get pModifier while creating MainBlock"
        Right o -> pure o
    let blockUndo = Undo (reverse $ foldl' prependToUndo [] localTxs)
                         pskUndo
                         (verUndo ^. _Wrapped . _neHead)
    lift $ blk <$ applyBlocksUnsafe (one (Right blk, blockUndo)) pModifier
  where
    onBrokenTopo = throwError "Topology of local transactions is broken!"
    onNoSsc = throwError "can't obtain SSC payload to create block"

createMainBlockPure
    :: Ssc ssc
    => BlockHeader ssc
    -> [(TxId, TxAux)]
    -> Maybe ProxySKEither
    -> SlotId
    -> [ProxySKSimple]
    -> SscPayload ssc
    -> SecretKey
    -> MainBlock ssc
createMainBlockPure prevHeader txs pSk sId psks sscData sk =
    mkMainBlock (Just prevHeader) sId sk pSk body extraH extraB
  where
    extraB = MainExtraBodyData (mkAttributes ())
    extraH = MainExtraHeaderData curProtocolVersion curSoftwareVersion (mkAttributes ())
    body = mkMainBody (fmap snd txs) sscData psks (UpdatePayload Nothing [])
