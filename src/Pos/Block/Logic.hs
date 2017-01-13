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

import           Control.Lens              (view, (^.), _1)
import           Control.Monad.Except      (ExceptT (ExceptT), runExceptT, throwError)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Default              (Default (def))
import qualified Data.HashMap.Strict       as HM
import           Data.List.NonEmpty        (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as T
import           Formatting                (build, int, ords, sformat, stext, (%))
import           Serokell.Util.Verify      (VerificationRes (..), formatAllErrors,
                                            isVerSuccess, verResToMonadError)
import           System.Wlog               (logDebug)
import           Universum

import           Pos.Block.Logic.Internal  (applyBlocksUnsafe, rollbackBlocksUnsafe,
                                            withBlkSemaphore, withBlkSemaphore_)
import           Pos.Constants             (curProtocolVersion, curSoftwareVersion, k)
import           Pos.Context               (NodeContext (ncSecretKey), getNodeContext,
                                            lrcActionOnEpochReason, readBlkSemaphore)
import           Pos.Crypto                (SecretKey, WithHash (WithHash), hash,
                                            shortHashF)
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.DB                    (DBError (..), MonadDB, getTipBlockHeader,
                                            loadHeadersWhile)
import qualified Pos.DB                    as DB
import qualified Pos.DB.GState             as GS
import qualified Pos.DB.Lrc                as LrcDB
import           Pos.Delegation.Logic      (delegationVerifyBlocks, getProxyMempool)
import           Pos.Lrc.Worker            (lrcSingleShotNoLock)
import           Pos.Slotting              (getCurrentSlot)
import           Pos.Ssc.Class             (Ssc (..), SscWorkersClass (..))
import           Pos.Ssc.Extra             (sscGetLocalPayload, sscVerifyBlocks)
import           Pos.Txp.Class             (getLocalTxsNUndo)
import           Pos.Txp.Logic             (txVerifyBlocks)
import           Pos.Types                 (Block, BlockHeader, Blund, EpochIndex,
                                            EpochOrSlot (..), GenesisBlock, HeaderHash,
                                            MainBlock, MainExtraBodyData (..),
                                            MainExtraHeaderData (..), ProxySKEither,
                                            ProxySKSimple, SlotId (..), TxAux, TxId,
                                            Undo (..), VerifyHeaderParams (..),
                                            blockHeader, difficultyL, epochIndexL,
                                            epochOrSlot, flattenEpochOrSlot, genesisHash,
                                            getEpochOrSlot, headerHash, headerHashG,
                                            headerSlot, mkGenesisBlock, mkMainBlock,
                                            mkMainBody, prevBlockL, topsortTxs,
                                            verifyHeader, verifyHeaders,
                                            vhpVerifyConsensus)
import qualified Pos.Types                 as Types
import           Pos.Util                  (inAssertMode, spanSafe, _neHead)
import           Pos.WorkMode              (WorkMode)


----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

-- | Common error message
tipMismatchMsg :: Text -> HeaderHash ssc -> HeaderHash ssc -> Text
tipMismatchMsg action storedTip attemptedTip =
    sformat
        ("Can't "%stext%" block because of tip mismatch (stored is "
         %shortHashF%", attempted is "%shortHashF%")")
        action storedTip attemptedTip


-- | Find lca headers and main chain, including oldest header's parent
-- hash. Headers passed are __newest first__.
lcaWithMainChain
    :: (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc) -> m (Maybe (HeaderHash ssc))
lcaWithMainChain headers@(h:|hs) =
    fmap fst . find snd <$>
        mapM (\hh -> (hh,) <$> GS.isBlockInMainChain hh)
             -- take hash of parent of last BlockHeader and convert all headers to hashes
             (map hash (h : hs) ++ [NE.last headers ^. prevBlockL])

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
        else return $ CHUseless "header is not for current slot"
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

-- | Classify headers received in response to 'GetHeaders'
-- message. Should be passed in newest-head order.
--
-- * If there are any errors in chain of headers, CHsInvalid is returned.
-- * If chain of headers is a valid continuation or alternative branch,
-- lca child is returned.
-- * If chain of headers forks from our main chain too much, CHsUseless
-- is returned, because paper suggests doing so.
classifyHeaders
    :: WorkMode ssc m
    => NonEmpty (BlockHeader ssc) -> m (ClassifyHeadersRes ssc)
classifyHeaders headers@(h:|hs) = do
    haveLast <- isJust <$> DB.getBlockHeader (hash $ NE.last headers)
    let headersValid = isVerSuccess $ verifyHeaders True $ h : hs
    if | not headersValid ->
             pure $ CHsInvalid "Header chain is invalid"
       | not haveLast ->
             pure $ CHsInvalid "Last block of the passed chain wasn't found locally"
       | otherwise -> processClassify
  where
    processClassify = do
        tipHeader <- view blockHeader <$> DB.getTipBlock
        let tipHash = tipHeader ^. headerHashG
        lcaHash <-
            fromMaybe (panic $ sformat ("lca should exist, our tip: "%shortHashF) tipHash) <$>
            lcaWithMainChain headers
        lca <-
            fromMaybe (panic $ sformat ("lca should be resolvable: "%shortHashF) lcaHash) <$>
            DB.getBlockHeader lcaHash
        -- depth in terms of slots, not difficulty
        let depthDiff =
                flattenEpochOrSlot tipHeader -
                flattenEpochOrSlot lca
        let lcaChild =
                fromMaybe (panic "processClassify@classifyHeaders") $
                find (\bh -> bh ^. prevBlockL == hash lca) (h:hs)
        pure $ if
            | hash lca == hash tipHeader -> CHsValid lcaChild
            | depthDiff < 0 -> panic "classifyHeaders@depthDiff is negative"
            | depthDiff > k ->
                  CHsUseless $
                  sformat ("Slot difference of (tip,lca) is "%int%
                           " which is more than k = "%int)
                          depthDiff (k :: Int)
            | otherwise -> CHsValid lcaChild

-- | Given a set of checkpoints to stop at, we take second header hash
-- block (or tip if latter is @Nothing@) and fetch the blocks until we
-- reach genesis block or one of checkpoints. Returned headers are
-- newest-first.
getHeadersFromManyTo
    :: (MonadDB ssc m, Ssc ssc)
    => [HeaderHash ssc] -> Maybe (HeaderHash ssc) -> m [BlockHeader ssc]
getHeadersFromManyTo checkpoints startM = do
    validCheckpoints <- catMaybes <$> mapM DB.getBlockHeader checkpoints
    tip <- GS.getTip
    let startFrom = fromMaybe tip startM
        neq = (/=) `on` getEpochOrSlot
        whileCond bh _ = all (neq bh) validCheckpoints
    headers <- reverse <$> loadHeadersWhile startFrom whileCond
    -- In case we didn't reach the very-first block we take one more
    -- because "until" predicate will stop us before we get
    -- checkpoint block and we do want to return it as well
    oneMore <- case headers of
        []       -> pure Nothing
        (last:_) -> DB.getBlockHeader $ last ^. prevBlockL
    pure $ reverse $ maybe identity (:) oneMore $ headers

-- | Given a starting point hash (we take tip if it's not in storage)
-- it returns not more than 'k' blocks distributed exponentially base
-- 2 relatively to the depth in the blockchain.
getHeadersOlderExp
    :: (MonadDB ssc m, Ssc ssc)
    => Maybe (HeaderHash ssc) -> m [HeaderHash ssc]
getHeadersOlderExp upto = do
    tip <- GS.getTip
    let upToReal = fromMaybe tip upto
        whileCond _ depth = depth <= k
    allHeaders <- reverse <$> loadHeadersWhile upToReal whileCond
    pure $ selectIndices (takeHashes allHeaders) twoPowers
  where
    -- Given list of headers newest first, maps it to their hashes
    takeHashes [] = []
    takeHashes headers@(x:_) =
        let prevHashes = map (view prevBlockL) headers
        in hash x : take (length prevHashes - 1) prevHashes
    -- Powers of 2
    twoPowers = (takeWhile (<k) $ 0 : 1 : iterate (*2) 2) ++ [k]
    -- Effectively do @!i@ for any @i@ from the index list applied to
    -- source list. Index list should be inreasing.
    selectIndices :: [a] -> [Int] -> [a]
    selectIndices elems ixs =
        let selGo _ [] _ = []
            selGo [] _ _ = []
            selGo ee@(e:es) ii@(i:is) skipped
                | skipped == i = e : selGo ee is skipped
                | otherwise    = selGo es ii $ succ skipped
        in selGo elems ixs 0

-- CSL-396 don't load all the blocks into memory at once
-- | Given @from@ and @to@ headers where @from@ is older (not strict)
-- than @to@, and valid chain in between can be found, headers in
-- range @[from..to]@ will be found. Header hashes are returned
-- oldest-first.
getHeadersFromToIncl
    :: forall ssc m .
       (MonadDB ssc m, Ssc ssc)
    => HeaderHash ssc -> HeaderHash ssc -> m (Maybe (NonEmpty (HeaderHash ssc)))
getHeadersFromToIncl older newer = runMaybeT $ do
    -- oldest and newest blocks do exist
    start <- MaybeT $ DB.getBlockHeader newer
    end <- MaybeT $ DB.getBlockHeader older
    guard $ flattenEpochOrSlot start >= flattenEpochOrSlot end
    let lowerBound = flattenEpochOrSlot end
    if newer == older
    then pure $ newer :| []
    else loadHeadersDo lowerBound (newer :| []) $ start ^. prevBlockL
  where
    loadHeadersDo
        :: Word64
        -> NonEmpty (HeaderHash ssc)
        -> HeaderHash ssc
        -> MaybeT m (NonEmpty (HeaderHash ssc))
    loadHeadersDo lowerBound hashes nextHash
        | nextHash == genesisHash = mzero
        | nextHash == older = pure $ nextHash <| hashes
        | otherwise = do
            nextHeader <- MaybeT $ DB.getBlockHeader nextHash
            guard $ flattenEpochOrSlot nextHeader > lowerBound
            loadHeadersDo lowerBound (nextHash <| hashes) (nextHeader ^. prevBlockL)


----------------------------------------------------------------------------
-- Blocks verify/apply/rollback
----------------------------------------------------------------------------

-- -- CHECK: @verifyBlocksLogic
-- -- #txVerifyBlocks
-- -- #sscVerifyBlocks
-- | Verify new blocks. Head is expected to be the oldest block. If
-- parent of head is not our tip, verification fails. This function
-- checks everything from block, including header, transactions,
-- delegation data, SSC data.
verifyBlocksPrefix
    :: WorkMode ssc m
    => NonEmpty (Block ssc) -> m (Either Text (NonEmpty Undo))
verifyBlocksPrefix blocks = runExceptT $ do
    curSlot <- getCurrentSlot
    tipBlk <- DB.getTipBlock
    verResToMonadError formatAllErrors $
        Types.verifyBlocks (Just curSlot) (tipBlk <| blocks)
    verResToMonadError formatAllErrors =<< sscVerifyBlocks False blocks
    txUndo <- ExceptT $ txVerifyBlocks blocks
    pskUndo <- ExceptT $ delegationVerifyBlocks blocks
    when (length txUndo /= length pskUndo) $
        throwError "Internal error of verifyBlocksPrefix: length of undos don't match"
    pure $ NE.map (uncurry Undo) $ NE.zip txUndo pskUndo

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
    => Bool -> NonEmpty (Block ssc) -> m (Either Text (HeaderHash ssc))
verifyAndApplyBlocks rollback = verifyAndApplyBlocksInternal True rollback

-- See the description for verifyAndApplyBlocks. This method also
-- parametrizes lrc calcultion which can be turned on/off using first
-- flag.
verifyAndApplyBlocksInternal
    :: (WorkMode ssc m, SscWorkersClass ssc)
    => Bool -> Bool -> NonEmpty (Block ssc) -> m (Either Text (HeaderHash ssc))
verifyAndApplyBlocksInternal lrc rollback blocks = runExceptT $ do
    tip <- GS.getTip
    let assumedTip = blocks ^. _neHead . prevBlockL
    when (tip /= assumedTip) $ throwError $
        tipMismatchMsg "verify and apply" tip assumedTip
    rollingVerifyAndApply [] (spanEpoch blocks)
  where
    spanEpoch = spanSafe ((==) `on` view epochIndexL)
    -- Applies as much blocks from failed prefix as possible. Argument
    -- indicates if at least some progress was done so we should
    -- return tip. Fail otherwise.
    applyAMAP e [] True            = throwError e
    applyAMAP _ [] False           = GS.getTip
    applyAMAP e (x:xs) nothingApplied = do
        let block = x:|[]
        lift (verifyBlocksPrefix block) >>= \case
            Left e' -> applyAMAP e' [] nothingApplied
            Right undo -> do
                lift $ applyBlocksUnsafe $ block `NE.zip` undo
                applyAMAP e xs False
    -- Rollbacks and returns an error
    failWithRollback e [] = throwError e
    failWithRollback e toRollback = do
        lift $ mapM_ rollbackBlocks toRollback
        throwError e
    rollingVerifyAndApply blunds (prefix,suffix) = do
        lift (verifyBlocksPrefix prefix) >>= \case
            Left failure | rollback -> failWithRollback failure blunds
            Left failure -> applyAMAP failure (NE.toList prefix) $ null blunds
            Right undos -> do
                let newBlunds = prefix `NE.zip` undos
                lift $ applyBlocksUnsafe newBlunds
                case suffix of
                    (genesis:xs) -> do
                        when lrc $ lift $ lrcSingleShotNoLock $ genesis ^. epochIndexL
                        rollingVerifyAndApply (NE.reverse newBlunds : blunds) $
                            spanEpoch $ genesis:|xs
                    [] -> GS.getTip


-- | Apply definitely valid sequence of blocks. At this point we must
-- have verified all predicates regarding block (including txs and ssc
-- data checks). We almost must have taken lock on block application
-- and ensured that chain is based on our tip. Blocks will be applied
-- per-epoch, calculating lrc when needed if flag is set.
applyBlocks
    :: forall ssc m . (WorkMode ssc m, SscWorkersClass ssc)
    => Bool -> NonEmpty (Blund ssc) -> m ()
applyBlocks calculateLrc blunds = do
    applyBlocksUnsafe prefix
    case suffix of
        (genesis:xs) -> do
            when calculateLrc $ lrcSingleShotNoLock $ genesis ^. _1 . epochIndexL
            applyBlocks calculateLrc $ genesis:|xs
        [] -> pass
  where
    (prefix,suffix) = spanSafe ((==) `on` view (_1 . epochIndexL)) blunds

-- | Rollbacks blocks. Head is to be current tip (newest first order).
rollbackBlocks :: (WorkMode ssc m) => NonEmpty (Blund ssc) -> m (Maybe Text)
rollbackBlocks blunds = do
    tip <- GS.getTip
    let firstToRollback = blunds ^. _neHead . _1 . headerHashG
    if tip /= firstToRollback
    then pure $ Just $ tipMismatchMsg "rollback" tip firstToRollback
    else rollbackBlocksUnsafe blunds $> Nothing

-- | Given a number of blocks to rollback on and apply then, does
-- it. Blocks to rollback are expected tip-first. To apply --
-- oldest-first.
applyWithRollback
    :: (WorkMode ssc m, SscWorkersClass ssc)
    => NonEmpty (Blund ssc)
    -> NonEmpty (Block ssc)
    -> m (Either Text (HeaderHash ssc))
applyWithRollback toRollback toApply = runExceptT $ do
    tip <- GS.getTip
    when (tip /= newestToRollback) $ do
        throwError (tipMismatchMsg "rollback in 'apply with rollback'" tip newestToRollback)
    lift $ rollbackBlocksUnsafe toRollback
    tipAfterRollback <- GS.getTip
    when (tipAfterRollback /= expectedTipApply) $ do
        lift $ applyBlocksUnsafe $ NE.reverse toRollback
        throwError (tipMismatchMsg "apply in 'apply with rollback'" tip newestToRollback)
    lift (verifyAndApplyBlocks True toApply) >>= \case
        -- We didn't succeed to apply blocks, so will apply
        -- rollbacked back.
        Left err -> do
            lift $ applyBlocks True $ NE.reverse toRollback
            throwError err
        Right tipHash  -> pure tipHash
  where
    expectedTipApply = toApply ^. _neHead . prevBlockL
    newestToRollback = toRollback ^. _neHead . _1 . headerHashG


----------------------------------------------------------------------------
-- GenesisBlock creation
----------------------------------------------------------------------------

-- | Create genesis block if necessary.
--
-- We create genesis block for current epoch when head of currently known
-- best chain is MainBlock corresponding to one of last `k` slots of
-- (i - 1)-th epoch. Main check is that epoch is `(last stored epoch +
-- 1)`, but we also don't want to create genesis block on top of blocks
-- from previous epoch which are not from last k slots, because it's
-- practically impossible for them to be valid.
createGenesisBlock
    :: forall ssc m.
       WorkMode ssc m
    => EpochIndex -> m (Maybe (GenesisBlock ssc))
createGenesisBlock epochIndex = do
    tipHeader <- getTipBlockHeader
    logDebug $ sformat msgFmt epochIndex tipHeader
    if shouldCreateGenesisBlock epochIndex $ getEpochOrSlot tipHeader
        then createGenesisBlockDo epochIndex
        else Nothing <$ logDebug "We shouldn't create genesis block"
  where
    msgFmt =
        "We are trying to create genesis block for "%ords %
        " epoch, our tip header is\n"%build

shouldCreateGenesisBlock :: EpochIndex -> EpochOrSlot -> Bool
-- Genesis block for 0-th epoch is hardcoded.
shouldCreateGenesisBlock 0 _ = False
shouldCreateGenesisBlock epoch headEpochOrSlot =
    doCheck $ epochOrSlot (`SlotId` 0) identity headEpochOrSlot
  where
    doCheck SlotId {..} = siEpoch == epoch - 1 && siSlot >= 5 * k

createGenesisBlockDo
    :: forall ssc m.
       WorkMode ssc m
    => EpochIndex -> m (Maybe (GenesisBlock ssc))
createGenesisBlockDo epoch = do
    leaders <- lrcActionOnEpochReason epoch
                   "there are no leaders"
                   LrcDB.getLeaders
    res <- withBlkSemaphore (createGenesisBlockCheckAgain leaders)
    res <$ inAssertMode (logDebug . sformat newTipFmt =<< readBlkSemaphore)
  where
    newTipFmt = "After creating GenesisBlock our tip is: "%shortHashF
    createGenesisBlockCheckAgain leaders tip = do
        let noHeaderMsg =
                "There is no header is DB corresponding to tip from semaphore"
        tipHeader <-
            maybe (throwM $ DBMalformed noHeaderMsg) pure =<<
            DB.getBlockHeader tip
        createGenesisBlockFinally leaders tip tipHeader
    createGenesisBlockFinally leaders tip tipHeader
        | shouldCreateGenesisBlock epoch (getEpochOrSlot tipHeader) = do
              let blk = mkGenesisBlock (Just tipHeader) epoch leaders
              let newTip = headerHash blk
              applyBlocksUnsafe (pure (Left blk, Undo [] [])) $> (Just blk, newTip)
        | otherwise = (Nothing, tip) <$ logShouldNot
    logShouldNot = logDebug
          "After we took lock for genesis block creation, we noticed that we shouldn't create it"
----------------------------------------------------------------------------
-- MainBlock creation
----------------------------------------------------------------------------

-- | Create a new main block on top of best chain if possible.
-- Block can be created if:
-- • we know genesis block for epoch from given SlotId
-- • last known block is not more than k slots away from
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
        tipHeader <- getTipBlockHeader
        logDebug $ sformat msgFmt tipHeader
        case canCreateBlock sId tipHeader of
            Nothing  -> convertRes tip <$>
                runExceptT (createMainBlockFinish sId pSk tipHeader)
            Just err -> return (Left err, tip)
    convertRes oldTip (Left e) = (Left e, oldTip)
    convertRes _ (Right blk)   = (Right blk, headerHash blk)

canCreateBlock :: SlotId -> BlockHeader ssc -> Maybe Text
canCreateBlock sId tipHeader
       | sId > maxSlotId =
           Just "slot id is too big, we don't know recent block"
       | (EpochOrSlot $ Right sId) < headSlot =
           Just "slot id is not biger than one from last known block"
       | otherwise = Nothing
  where
    addKSafe si = si {siSlot = min (6 * k - 1) (siSlot si + k)}
    headSlot = getEpochOrSlot tipHeader
    maxSlotId = addKSafe $ epochOrSlot (`SlotId` 0) identity headSlot

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
    let blockUndo = Undo (reverse $ foldl' prependToUndo [] localTxs) pskUndo
    lift $ blk <$ applyBlocksUnsafe (pure (Right blk, blockUndo))
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
    -- TODO [CSL-351] inlclude proposal, votes into block
    extraB = MainExtraBodyData (mkAttributes ()) Nothing []
    extraH = MainExtraHeaderData curProtocolVersion curSoftwareVersion (mkAttributes ())
    body = mkMainBody (fmap snd txs) sscData psks
