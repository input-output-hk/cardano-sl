{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
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
       , verifyAndApplyBlocks
       , rollbackBlocks
       , applyWithRollback
       , createGenesisBlock
       , createMainBlock
       ) where

import           Control.Lens               ((-=), (.=), _Wrapped)
import           Control.Monad.Catch        (try)
import           Control.Monad.Except       (ExceptT (ExceptT), MonadError (throwError),
                                             runExceptT, withExceptT)
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT), runMaybeT)
import           Data.Default               (Default (def))
import qualified Data.HashMap.Strict        as HM
import           Data.List.NonEmpty         ((<|))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T
import           Formatting                 (build, int, ords, sformat, stext, (%))
import           Paths_cardano_sl           (version)
import           Serokell.Data.Memory.Units (toBytes)
import           Serokell.Util.Text         (listJson)
import           Serokell.Util.Verify       (VerificationRes (..), formatAllErrors,
                                             isVerSuccess, verResToMonadError)
import           System.Wlog                (CanLog, HasLoggerName, logDebug, logInfo)
import           Universum

import qualified Pos.Binary.Class           as Bi
import           Pos.Block.Logic.Internal   (applyBlocksUnsafe, rollbackBlocksUnsafe,
                                             withBlkSemaphore, withBlkSemaphore_)
import           Pos.Block.Types            (Blund, Undo (..))
import           Pos.Constants              (blkSecurityParam, curSoftwareVersion,
                                             epochSlots, lastKnownBlockVersion,
                                             recoveryHeadersMessage, slotSecurityParam)
import           Pos.Context                (NodeContext (ncNodeParams), getNodeContext,
                                             lrcActionOnEpochReason, npSecretKey)
import           Pos.Crypto                 (SecretKey, WithHash (WithHash), hash,
                                             shortHashF)
import           Pos.Data.Attributes        (mkAttributes)
import           Pos.DB                     (DBError (..), MonadDB)
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import qualified Pos.DB.GState              as GS
import           Pos.Delegation.Logic       (delegationVerifyBlocks, getProxyMempool)
import           Pos.Exception              (assertionFailed, reportFatalError)
import qualified Pos.Lrc.DB                 as LrcDB
import           Pos.Lrc.Error              (LrcError (..))
import           Pos.Lrc.Worker             (lrcSingleShotNoLock)
import           Pos.Reporting              (reportingFatal)
import           Pos.Slotting.Class         (getCurrentSlot)
import           Pos.Ssc.Class              (Ssc (..), SscHelpersClass,
                                             SscWorkersClass (..))
import           Pos.Ssc.Extra              (sscGetLocalPayload, sscVerifyBlocks)
import           Pos.Txp.Core               (TxAux, TxId, mkTxPayload, topsortTxs)
import           Pos.Txp.Logic              (txVerifyBlocks)
import           Pos.Txp.MemState           (getLocalTxsNUndo)
import           Pos.Types                  (Block, BlockHeader, EpochIndex,
                                             EpochOrSlot (..), GenesisBlock, HeaderHash,
                                             MainBlock, MainExtraBodyData (..),
                                             MainExtraHeaderData (..), ProxySKEither,
                                             ProxySKHeavy, SlotId (..), SlotLeaders,
                                             VerifyHeaderParams (..), blockHeader,
                                             blockLeaders, difficultyL, epochIndexL,
                                             epochOrSlot, flattenSlotId, genesisHash,
                                             getEpochOrSlot, headerHash, headerHashG,
                                             headerSlot, mkGenesisBlock, mkMainBlock,
                                             prevBlockL, verifyHeader, verifyHeaders,
                                             vhpVerifyConsensus)
import qualified Pos.Types                  as Types
import           Pos.Update.Core            (UpdatePayload (..))
import qualified Pos.Update.DB              as UDB
import           Pos.Update.Logic           (usCanCreateBlock, usPreparePayload,
                                             usVerifyBlocks)
import           Pos.Update.Poll            (PollModifier)
import           Pos.Util                   (NE, NewestFirst (..), OldestFirst (..),
                                             inAssertMode, maybeThrow, neZipWith3,
                                             spanSafe, toNewestFirst, toOldestFirst,
                                             _neHead, _neLast)
import           Pos.WorkMode               (WorkMode)

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

-- Usually in this method oldest header is LCA, so it can be optimized
-- by traversing from older to newer.
-- | Find LCA of headers list and main chain, including oldest
-- header's parent hash. Iterates from newest to oldest until meets
-- first header that's in main chain. O(n).
lcaWithMainChain
    :: (WorkMode ssc m)
    => OldestFirst NE (BlockHeader ssc) -> m (Maybe HeaderHash)
lcaWithMainChain headers =
    lcaProceed Nothing $
    oldestParent <| fmap headerHash (getOldestFirst headers)
  where
    oldestParent :: HeaderHash
    oldestParent = headers ^. _Wrapped . _neHead . prevBlockL
    lcaProceed prevValue (h :| others) = do
        inMain <- GS.isBlockInMainChain h
        case (others, inMain) of
            (_, False)   -> pure prevValue
            ([], True)   -> pure $ Just h
            (x:xs, True) -> lcaProceed (Just h) (x :| xs)

----------------------------------------------------------------------------
-- Headers
----------------------------------------------------------------------------

-- | Result of single (new) header classification.
data ClassifyHeaderRes
    = CHContinues
      -- ^ Header is direct continuation of main chain (i.e. its
      -- parent is our tip).
    | CHAlternative
      -- ^ Header continues main or alternative chain, it's more
      -- difficult than tip.
    | CHUseless !Text
      -- ^ Header is useless.
    | CHInvalid !Text
      -- ^ Header is invalid.
    deriving (Show)

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
    tipBlock <- DB.getTipBlock
    let tipEoS= getEpochOrSlot tipBlock
    let newHeaderEoS = getEpochOrSlot header
    let newHeaderSlot = header ^. headerSlot
    let tip = headerHash tipBlock
    -- First of all we check whether header is from current slot and
    -- ignore it if it's not.
    pure $ if
        -- Checks on slots
        | maybe False (newHeaderSlot >) curSlot ->
            CHUseless $ sformat
               ("header is for future slot: our is "%build%
                ", header's is "%build)
               curSlot newHeaderSlot
        | newHeaderEoS <= tipEoS ->
            CHUseless $ sformat
               ("header's slot "%build%
                " is less or equal than our tip's slot "%build)
               newHeaderEoS tipEoS
        -- If header's parent is our tip, we verify it against tip's header.
        | tip == header ^. prevBlockL ->
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
        | tipBlock ^. difficultyL < header ^. difficultyL -> CHAlternative
        -- If header can't continue main chain and is not more
        -- difficult than main chain, it's useless.
        | otherwise ->
            CHUseless $
            "header doesn't continue main chain and is not more difficult"

-- | Result of multiple headers classification.
data ClassifyHeadersRes ssc
    = CHsValid (BlockHeader ssc) -- ^ Header list can be applied, LCA child attached.
    | CHsUseless !Text           -- ^ Header is useless.
    | CHsInvalid !Text           -- ^ Header is invalid.
    deriving (Show)

-- | Classify headers received in response to 'GetHeaders' message.
--
-- * If there are any errors in chain of headers, CHsInvalid is returned.
-- * If chain of headers is a valid continuation or alternative branch,
-- lca child is returned.
-- * If chain of headers forks from our main chain too much, CHsUseless
-- is returned, because paper suggests doing so.
classifyHeaders
    :: forall ssc m.
       WorkMode ssc m
    => NewestFirst NE (BlockHeader ssc) -> m (ClassifyHeadersRes ssc)
classifyHeaders headers = do
    tipHeader <- DB.getTipBlockHeader @ssc
    let tip = headerHash tipHeader
    haveOldestParent <- isJust <$> DB.getBlockHeader @ssc oldestParentHash
    let headersValid = isVerSuccess $ verifyHeaders True (headers & _Wrapped %~ toList)
    if | not headersValid ->
             pure $ CHsInvalid "Header chain is invalid"
       | not haveOldestParent ->
             pure $ CHsInvalid $
             "Didn't manage to find block corresponding to parent " <>
             "of oldest element in chain (should be one of checkpoints)"
       | newestHash == headerHash tip ->
             pure $ CHsUseless "Newest hash is the same as our tip"
       | newestHeader ^. difficultyL <= tipHeader ^. difficultyL ->
             pure $ CHsUseless "Newest hash difficulty is not greater than our tip's"
       | otherwise -> fromMaybe uselessGeneral <$> processClassify tipHeader
  where
    newestHeader = headers ^. _Wrapped . _neHead
    newestHash = headerHash newestHeader
    oldestParentHash = headers ^. _Wrapped . _neLast . prevBlockL
    uselessGeneral =
        CHsUseless "Couldn't find lca -- maybe db state updated in the process"
    processClassify tipHeader = runMaybeT $ do
        lift $ logDebug $
            sformat ("Classifying headers: "%listJson) $ map (view headerHashG) headers
        lca <-
            MaybeT . DB.getBlockHeader =<<
            MaybeT (lcaWithMainChain $ toOldestFirst headers)
        let depthDiff = tipHeader ^. difficultyL - lca ^. difficultyL
        lcaChild <- MaybeT $ pure $
            find (\bh -> bh ^. prevBlockL == headerHash lca) headers
        pure $ if
            | hash lca == hash tipHeader -> CHsValid lcaChild
            | depthDiff < 0 -> error "classifyHeaders@depthDiff is negative"
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
    :: forall ssc m.
       (MonadDB m, SscHelpersClass ssc, CanLog m, HasLoggerName m)
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
        mapM (DB.getBlockHeader @ssc) (toList checkpoints)
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
                    (toList validCheckpoints)
        lift $ logDebug $ "getHeadersFromManyTo: got checkpoints in main chain"
        let lowestCheckpoint =
                maximumBy (comparing getEpochOrSlot) inMainCheckpoints
            loadUpCond _ h = h < recoveryHeadersMessage
        up <- lift $ GS.loadHeadersUpWhile lowestCheckpoint loadUpCond
        res <- MaybeT $ pure $ _Wrapped nonEmpty (toNewestFirst $ over _Wrapped (drop 1) up)
        lift $ logDebug $ "getHeadersFromManyTo: loaded non-empty list of headers, returning"
        pure res

-- | Given a starting point hash (we take tip if it's not in storage)
-- it returns not more than 'blkSecurityParam' blocks distributed
-- exponentially base 2 relatively to the depth in the blockchain.
getHeadersOlderExp
    :: forall ssc m.
       (MonadDB m, SscHelpersClass ssc)
    => Maybe HeaderHash -> m (OldestFirst [] HeaderHash)
getHeadersOlderExp upto = do
    tip <- GS.getTip
    let upToReal = fromMaybe tip upto
    -- Using 'blkSecurityParam + 1' because fork can happen on k+1th one.
    allHeaders <-
        toOldestFirst <$> DB.loadHeadersByDepth @ssc (blkSecurityParam + 1) upToReal
    pure $ OldestFirst $
        selectIndices
            (getOldestFirst (map headerHash allHeaders))
            (twoPowers $ length allHeaders)
  where
    -- Powers of 2
    twoPowers n
        | n < 0 = error $ "getHeadersOlderExp#twoPowers called w/" <> show n
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
       (MonadDB m, SscHelpersClass ssc)
    => HeaderHash -> HeaderHash -> m (Maybe (OldestFirst NE HeaderHash))
getHeadersFromToIncl older newer = runMaybeT . fmap OldestFirst $ do
    -- oldest and newest blocks do exist
    start <- MaybeT $ DB.getBlockHeader @ssc newer
    end   <- MaybeT $ DB.getBlockHeader @ssc older
    guard $ getEpochOrSlot start >= getEpochOrSlot end
    let lowerBound = getEpochOrSlot end
    if newer == older
    then pure $ one newer
    else loadHeadersDo lowerBound (one newer) $ start ^. prevBlockL
  where
    loadHeadersDo
        :: EpochOrSlot
        -> NonEmpty HeaderHash
        -> HeaderHash
        -> MaybeT m (NonEmpty HeaderHash)
    loadHeadersDo lowerBound hashes nextHash
        | nextHash == genesisHash = mzero
        | nextHash == older = pure $ nextHash <| hashes
        | otherwise = do
            nextHeader <- MaybeT $ (DB.getBlockHeader @ssc) nextHash
            guard $ getEpochOrSlot nextHeader > lowerBound
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
-- header, transactions, delegation data, SSC data, US data.
verifyBlocksPrefix
    :: forall ssc m.
       WorkMode ssc m
    => OldestFirst NE (Block ssc)
    -> m (Either Text (OldestFirst NE Undo, PollModifier))
verifyBlocksPrefix blocks = runExceptT $ do
    curSlot <- getCurrentSlot
    tipBlk <- DB.getTipBlock @ssc
    when (headerHash tipBlk /= blocks ^. _Wrapped . _neHead . prevBlockL) $
        throwError "the first block isn't based on the tip"
    leaders <-
        lrcActionOnEpochReason
        headEpoch
        (sformat
         ("Block.Logic#verifyBlocksPrefix: there are no leaders for epoch "%build) headEpoch)
        LrcDB.getLeaders
    case blocks ^. _Wrapped . _neHead of
        (Left block) ->
            when (block ^. blockLeaders /= leaders) $
                throwError "Genesis block leaders don't match with LRC-computed"
        _ -> pass
    (bv, bvd) <- UDB.getAdoptedBVFull
    verResToMonadError formatAllErrors $
        Types.verifyBlocks curSlot bvd (Just leaders) (Just bv) blocks
    _ <- withExceptT pretty $ sscVerifyBlocks blocks
    txUndo <- withExceptT pretty $ txVerifyBlocks blocks
    pskUndo <- ExceptT $ delegationVerifyBlocks blocks
    (pModifier, usUndos) <- withExceptT pretty $ usVerifyBlocks blocks
    when (length txUndo /= length pskUndo) $
        throwError "Internal error of verifyBlocksPrefix: lengths of undos don't match"
    pure ( OldestFirst $ neZipWith3 Undo
               (getOldestFirst txUndo)
               (getOldestFirst pskUndo)
               (getOldestFirst usUndos)
         , pModifier)
  where
    headEpoch = blocks ^. _Wrapped . _neHead . epochIndexL

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
verifyAndApplyBlocks rollback =
    reportingFatal version . verifyAndApplyBlocksInternal True rollback

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
    spanEpoch (OldestFirst (b@(Left _):|xs)) = (OldestFirst $ b:|[], OldestFirst xs)
    spanEpoch x                              = spanTail x
    spanTail = over _1 OldestFirst . over _2 OldestFirst .  -- wrap both results
                spanSafe ((==) `on` view epochIndexL) .      -- do work
                getOldestFirst                               -- unwrap argument
    -- Applies as much blocks from failed prefix as possible. Argument
    -- indicates if at least some progress was done so we should
    -- return tip. Fail otherwise.
    applyAMAP e (OldestFirst []) True                   = throwError e
    applyAMAP _ (OldestFirst []) False                  = GS.getTip
    applyAMAP e (OldestFirst (block:xs)) nothingApplied =
        lift (verifyBlocksPrefix (one block)) >>= \case
            Left e' -> applyAMAP e' (OldestFirst []) nothingApplied
            Right (OldestFirst (undo :| []), pModifier) -> do
                lift $ applyBlocksUnsafe (one (block, undo)) (Just pModifier)
                applyAMAP e (OldestFirst xs) False
            Right _ -> error "verifyAndApplyBlocksInternal: applyAMAP: \
                             \verification of one block produced more than one undo"
    -- Rollbacks and returns an error
    failWithRollback
        :: Text
        -> [NewestFirst NE (Blund ssc)]
        -> ExceptT Text m HeaderHash
    failWithRollback e toRollback = do
        lift $ mapM_ rollbackBlocks toRollback
        throwError e
    -- Calculates LRC if it's needed (no lock)
    calculateLrc epochIx =
        when lrc $ lift $ lrcSingleShotNoLock epochIx
    -- This function tries to apply a new portion of blocks (prefix
    -- and suffix). It also has aggregating parameter blunds which is
    -- collected to rollback blocks if correspondent flag is on. First
    -- list is packs of blunds -- head of this list represents blund
    -- to rollback first. This function also tries to apply as much as
    -- possible if the flag is on.
    rollingVerifyAndApply
        :: [NewestFirst NE (Blund ssc)]
        -> (OldestFirst NE (Block ssc), OldestFirst [] (Block ssc))
        -> ExceptT Text m HeaderHash
    rollingVerifyAndApply blunds (prefix, suffix) = do
        let prefixHead = prefix ^. _Wrapped . _neHead
        when (isLeft prefixHead) $ calculateLrc (prefixHead ^. epochIndexL)
        lift (verifyBlocksPrefix prefix) >>= \case
            Left failure
                | rollback  -> failWithRollback failure blunds
                | otherwise ->
                      applyAMAP failure
                                   (over _Wrapped toList prefix)
                                   (null blunds)
            Right (undos, pModifier) -> do
                let newBlunds = OldestFirst $ getOldestFirst prefix `NE.zip`
                                              getOldestFirst undos
                lift $ applyBlocksUnsafe newBlunds (Just pModifier)
                case getOldestFirst suffix of
                    [] -> GS.getTip
                    (genesis:xs) -> do
                        rollingVerifyAndApply (toNewestFirst newBlunds : blunds) $
                            spanEpoch (OldestFirst (genesis:|xs))

-- | Apply definitely valid sequence of blocks. At this point we must
-- have verified all predicates regarding block (including txs and ssc
-- data checks). We also must have taken lock on block application
-- and ensured that chain is based on our tip. Blocks will be applied
-- per-epoch, calculating lrc when needed if flag is set.
applyBlocks
    :: forall ssc m . (WorkMode ssc m, SscWorkersClass ssc)
    => Bool -> Maybe PollModifier -> OldestFirst NE (Blund ssc) -> m ()
applyBlocks calculateLrc pModifier blunds = do
    when (isLeft prefixHead && calculateLrc) $
        -- Hopefully this lrc check is never triggered -- because
        -- caller most definitely should have computed lrc to verify
        -- the sequence beforehand.
        lrcSingleShotNoLock (prefixHead ^. epochIndexL)
    applyBlocksUnsafe prefix pModifier
    case getOldestFirst suffix of
        []           -> pass
        (genesis:xs) -> applyBlocks calculateLrc pModifier (OldestFirst (genesis:|xs))
  where
    prefixHead = prefix ^. _Wrapped . _neHead . _1
    (prefix, suffix) = spanEpoch blunds
    -- this version is different from one in verifyAndApply subtly,
    -- but they can be merged with some struggle.
    spanEpoch (OldestFirst (b@((Left _),_):|xs)) = (OldestFirst $ b:|[], OldestFirst xs)
    spanEpoch x                                  = spanTail x
    spanTail = over _1 OldestFirst . over _2 OldestFirst .
               spanSafe ((==) `on` view (_1 . epochIndexL)) .
               getOldestFirst

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
applyWithRollback toRollback toApply = reportingFatal version $ runExceptT $ do
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
    applyBack = lift $ applyBlocks True Nothing reApply
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
createGenesisBlock epoch = reportingFatal version $ do
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
    tipHeader <- maybeThrow (DBMalformed noHeaderMsg) =<< DB.getBlockHeader tip
    logDebug $ sformat msgTryingFmt epoch tipHeader
    createGenesisBlockFinally tipHeader
  where
    createGenesisBlockFinally tipHeader
        | shouldCreateGenesisBlock epoch (getEpochOrSlot tipHeader) = do
            let blk = mkGenesisBlock (Just tipHeader) epoch leaders
            let newTip = headerHash blk
            runExceptT (usVerifyBlocks (one (Left blk))) >>= \case
                Left err -> reportFatalError $ pretty err
                Right (pModifier, usUndos) -> do
                    let undo = def {undoUS = usUndos ^. _Wrapped . _neHead}
                    applyBlocksUnsafe (one (Left blk, undo)) (Just pModifier) $>
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
createMainBlock sId pSk =
    reportingFatal version $ withBlkSemaphore createMainBlockDo
  where
    msgFmt = "We are trying to create main block, our tip header is\n"%build
    createMainBlockDo tip = do
        tipHeader <- DB.getTipBlockHeader
        logInfo $ sformat msgFmt tipHeader
        canWrtUs <- usCanCreateBlock
        case (canCreateBlock sId tipHeader, canWrtUs) of
            (_, False) ->
                return (Left "this software can't create block", tip)
            (Nothing, True)  -> convertRes tip <$>
                runExceptT (createMainBlockFinish sId pSk tipHeader)
            (Just err, True) -> return (Left err, tip)
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
    (localTxs, txUndo) <- getLocalTxsNUndo
    sscData <- sscGetLocalPayload @ssc slotId
    usPayload <- note onNoUS =<< lift (usPreparePayload slotId)
    (localPSKs, pskUndo) <- lift getProxyMempool
    let convertTx (txId, (tx, _, _)) = WithHash tx txId
    sortedTxs <- maybe onBrokenTopo pure $ topsortTxs convertTx localTxs
    sk <- npSecretKey . ncNodeParams <$> getNodeContext
    -- for now let's be cautious and not generate blocks that are larger than
    -- maxBlockSize/4
    sizeLimit <- fromIntegral . toBytes . (`div` 4) <$> UDB.getMaxBlockSize
    blk <- createMainBlockPure sizeLimit prevHeader sortedTxs pSk
                  slotId localPSKs sscData usPayload sk
    let prependToUndo undos tx =
            fromMaybe (error "Undo for tx not found")
                      (HM.lookup (fst tx) txUndo) : undos
    lift $ inAssertMode $ verifyBlocksPrefix (one (Right blk)) >>= \case
        Left err ->
            assertionFailed $ sformat ("We've created bad block: "%stext) err
        Right _ -> pass
    (pModifier,verUndo) <- runExceptT (usVerifyBlocks (one (Right blk))) >>= \case
        Left _ -> throwError "Couldn't get pModifier while creating MainBlock"
        Right o -> pure o
    let blockUndo = Undo (reverse $ foldl' prependToUndo [] localTxs)
                         pskUndo
                         (verUndo ^. _Wrapped . _neHead)
    !() <- (blockUndo `deepseq` blk) `deepseq` pure ()
    logDebug "Created main block/undos, applying"
    lift $ blk <$ applyBlocksUnsafe (one (Right blk, blockUndo)) (Just pModifier)
  where
    onBrokenTopo = throwError "Topology of local transactions is broken!"
    onNoUS = "can't obtain US payload to create block"

createMainBlockPure
    :: (MonadError Text m, SscHelpersClass ssc)
    => Word64                   -- ^ Block size limit (TODO: imprecise)
    -> BlockHeader ssc
    -> [(TxId, TxAux)]
    -> Maybe ProxySKEither
    -> SlotId
    -> [ProxySKHeavy]
    -> SscPayload ssc
    -> UpdatePayload
    -> SecretKey
    -> m (MainBlock ssc)
createMainBlockPure limit prevHeader txs pSk sId psks sscData usPayload sk =
    flip evalStateT limit $ do
        -- account for block header and serialization overhead, etc; also
        -- include all SSC data because a) deciding is hard and b) we don't
        -- yet have a way to strip generic SSC data
        let musthaveBody = Types.MainBody
                (fromMaybe (error "createMainBlockPure: impossible") $ mkTxPayload mempty)
                sscData [] def
        musthaveBlock <-
            either throwError pure $
            mkMainBlock (Just prevHeader) sId sk pSk musthaveBody extraH extraB
        count musthaveBlock
        -- include delegation certificates and US payload
        let prioritizeUS = even (flattenSlotId sId)
        (psks', usPayload') <-
            if prioritizeUS then do
                usPayload' <- includeUSPayload
                psks' <- takeSome psks
                return (psks', usPayload')
            else do
                psks' <- takeSome psks
                usPayload' <- includeUSPayload
                return (psks', usPayload')
        -- include transactions
        txs' <- takeSome (map snd txs)
        -- return the resulting block
        txPayload <- either throwError pure $ mkTxPayload txs'
        let body = Types.MainBody txPayload sscData psks' usPayload'
        maybe (error "Coudln't create block") return $
              mkMainBlock (Just prevHeader) sId sk pSk body extraH extraB
  where
    count x = identity -= fromIntegral (length (Bi.encode x))
    -- take from a list until the limit is exhausted or the list ends
    takeSome lst = do
        let go lim [] = (lim, [])
            go lim (x:xs) =
                let len = fromIntegral $ length (Bi.encode x)
                in if len > lim
                     then (lim, [])
                     else over _2 (x:) $ go (lim - len) xs
        (lim', pref) <- go <$> use identity <*> pure lst
        identity .= lim'
        return pref
    -- include UpdatePayload if we have space for it (not very precise
    -- because we have already counted empty payload but whatever)
    includeUSPayload = do
        lim <- use identity
        let len = fromIntegral $ length (Bi.encode usPayload)
        if len <= lim
            then (identity -= len) >> return usPayload
            else return def
    -- other stuff
    extraB = MainExtraBodyData (mkAttributes ())
    extraH =
        MainExtraHeaderData
            lastKnownBlockVersion
            curSoftwareVersion
            (mkAttributes ())
