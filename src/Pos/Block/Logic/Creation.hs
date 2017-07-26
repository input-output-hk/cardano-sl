{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Block creation logic.

module Pos.Block.Logic.Creation
       ( createGenesisBlockAndApply
       , createMainBlockAndApply
       , createMainBlockInternal

       -- * Internals
       , RawPayload (..)
       , createMainBlockPure
       ) where

import           Universum

import           Control.Lens               (uses, (-=), (.=), _Wrapped)
import           Control.Monad.Catch        (try)
import           Control.Monad.Except       (MonadError (throwError), runExceptT)
import           Data.Default               (Default (def))
import           Ether.Internal             (HasLens (..))
import           Formatting                 (build, fixed, ords, sformat, stext, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           System.Wlog                (WithLogger, logDebug, logError, logInfo)

import           Pos.Binary.Class           (biSize)
import           Pos.Block.Core             (BlockHeader, GenesisBlock, MainBlock,
                                             MainBlockchain, mkGenesisBlock, mkMainBlock)
import qualified Pos.Block.Core             as BC
import           Pos.Block.Logic.Internal   (MonadBlockApply, applyBlocksUnsafe,
                                             normalizeMempool)
import           Pos.Block.Logic.Util       (calcChainQualityM, withBlkSemaphore)
import           Pos.Block.Logic.VAR        (verifyBlocksPrefix)
import           Pos.Block.Slog             (HasSlogContext (..))
import           Pos.Constants              (chainQualityThreshold, epochSlots)
import           Pos.Context                (BlkSemaphore, HasPrimaryKey, getOurSecretKey,
                                             lrcActionOnEpochReason)
import           Pos.Core                   (Blockchain (..), EpochIndex,
                                             EpochOrSlot (..), HeaderHash, SlotId (..),
                                             SlotLeaders, epochIndexL, flattenSlotId,
                                             getEpochOrSlot, headerHash)
import           Pos.Crypto                 (SecretKey, WithHash (WithHash))
import           Pos.DB                     (DBError (..))
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import           Pos.Delegation             (DelegationVar, DlgPayload (getDlgPayload),
                                             ProxySKBlockInfo, clearDlgMemPool,
                                             getDlgMempool, mkDlgPayload)
import           Pos.Exception              (assertionFailed, reportFatalError)
import           Pos.Lrc                    (LrcContext, LrcError (..))
import qualified Pos.Lrc.DB                 as LrcDB
import           Pos.Reporting              (reportMisbehaviourSilent, reportingFatal)
import           Pos.Ssc.Class              (Ssc (..), SscHelpersClass (sscDefaultPayload, sscStripPayload),
                                             SscLocalDataClass)
import           Pos.Ssc.Extra              (MonadSscMem, sscGetLocalPayload,
                                             sscResetLocal)
import           Pos.Txp.Core               (TxAux (..), emptyTxPayload, mkTxPayload,
                                             topsortTxs)
import           Pos.Txp.MemState           (MonadTxpMem, clearTxpMemPool, getLocalTxs)
import           Pos.Update                 (UpdateContext)
import           Pos.Update.Core            (UpdatePayload (..))
import qualified Pos.Update.DB              as UDB
import           Pos.Update.Logic           (clearUSMemPool, usCanCreateBlock,
                                             usPreparePayload)
import           Pos.Util                   (maybeThrow, _neHead)
import           Pos.Util.Util              (leftToPanic)
import           Pos.WorkMode.Class         (TxpExtra_TMP)

-- | A set of constraints necessary to create a block from mempool.
type MonadCreateBlock ssc ctx m
     = ( MonadReader ctx m
       , HasPrimaryKey ctx
       , HasSlogContext ctx -- to check chain quality
       , WithLogger m
       , DB.MonadBlockDB ssc m
       , MonadIO m
       , MonadMask m
       , HasLens LrcContext ctx LrcContext

       -- Mempools
       , HasLens DelegationVar ctx DelegationVar
       , MonadTxpMem TxpExtra_TMP ctx m
       , HasLens UpdateContext ctx UpdateContext
       , MonadSscMem ssc ctx m
       , SscLocalDataClass ssc
       )

----------------------------------------------------------------------------
-- GenesisBlock creation
----------------------------------------------------------------------------

-- | Create genesis block if necessary and apply it.
--
-- We can /try/ to create a genesis block at any moment. However, it
-- only makes sense to do it if the following conditions are met:
--
-- • our tip is a 'MainBlock' and its epoch is less than the given
--   epoch by one;
-- • chain quality is at least 0.5. To be more precise, it means that
--   there are at least `blkSecurityParam` blocks in the last
--   'slotSecurityParam' slots. If this condition is violated, it means
--   that we are either desynchronized\/eclipsed\/attacked or that
--   important security assumption is violated globally.
--   In the former case, it doesn't make sense to create a block.
--   In the latter case, we want the system to stop completely, rather
--   than running in insecure mode.
createGenesisBlockAndApply ::
       forall ssc ctx m.
       ( MonadCreateBlock ssc ctx m
       , MonadBlockApply ssc ctx m
       , HasLens BlkSemaphore ctx BlkSemaphore
       )
    => EpochIndex
    -> m (Maybe (GenesisBlock ssc))
-- Genesis block for 0-th epoch is hardcoded.
createGenesisBlockAndApply 0 = pure Nothing
createGenesisBlockAndApply epoch =
    reportingFatal $
    try (lrcActionOnEpochReason epoch "there are no leaders" LrcDB.getLeaders) >>= \case
        Left UnknownBlocksForLrc ->
            Nothing <$ logInfo "createGenesisBlock: not enough blocks for LRC"
        Left err -> throwM err
        Right leaders -> withBlkSemaphore (createGenesisBlockDo epoch leaders)

createGenesisBlockDo
    :: forall ssc ctx m.
       (MonadCreateBlock ssc ctx m, MonadBlockApply ssc ctx m)
    => EpochIndex
    -> SlotLeaders
    -> HeaderHash
    -> m (Maybe (GenesisBlock ssc), HeaderHash)
createGenesisBlockDo epoch leaders tip = do
    let noHeaderMsg =
            "There is no header is DB corresponding to tip from semaphore"
    tipHeader <- maybeThrow (DBMalformed noHeaderMsg) =<< DB.blkGetHeader tip
    logDebug $ sformat msgTryingFmt epoch tipHeader
    shouldCreate tipHeader >>= \case
        False -> (Nothing, tip) <$ logShouldNot
        True -> actuallyCreate tipHeader
  where
    shouldCreate (Left _) = pure False
    -- This is true iff tip is from 'epoch' - 1 and last
    -- 'blkSecurityParam' blocks fully fit into last
    -- 'slotSecurityParam' slots from 'epoch' - 1.
    shouldCreate (Right mb)
        | mb ^. epochIndexL /= epoch - 1 = pure False
        | otherwise =
            (chainQualityThreshold @Double <=) <$>
            calcChainQualityM (flattenSlotId $ SlotId epoch minBound)
    actuallyCreate tipHeader = do
        let blk = mkGenesisBlock (Just tipHeader) epoch leaders
        let newTip = headerHash blk
        verifyBlocksPrefix (one (Left blk)) >>= \case
            Left err -> reportFatalError $ pretty err
            Right (undos, pollModifier) -> do
                let undo = undos ^. _Wrapped . _neHead
                applyBlocksUnsafe (one (Left blk, undo)) (Just pollModifier)
                normalizeMempool
                pure (Just blk, newTip)
    logShouldNot =
        logDebug
            "After we took lock for genesis block creation, we noticed that we shouldn't create it"
    msgTryingFmt =
        "We are trying to create genesis block for " %ords %
        " epoch, our tip header is\n" %build

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

-- | Create a new main block on top of our tip if possible and apply it.
-- Block can be created if:
-- • our software is not obsolete (see 'usCanCreateBlock');
-- • our tip's slot is less than the slot for which we want to create a block;
-- • there are at least 'blkSecurityParam' blocks in the last
-- 'slotSecurityParam' slots prior to the given slot (i. e. chain quality
-- is decent).
--
-- In theory we can create main block even if chain quality is
-- bad. See documentation of 'createGenesisBlock' which explains why
-- we don't create blocks in such cases.
createMainBlockAndApply ::
       forall ssc ctx m.
       ( MonadCreateBlock ssc ctx m
       , MonadBlockApply ssc ctx m
       , HasLens BlkSemaphore ctx BlkSemaphore
       )
    => SlotId
    -> ProxySKBlockInfo
    -> m (Either Text (MainBlock ssc))
createMainBlockAndApply sId pske =
    reportingFatal $ withBlkSemaphore createAndApply
  where
    createAndApply tip =
        createMainBlockInternal sId pske >>= \case
            Left reason -> pure (Left reason, tip)
            Right blk -> convertRes <$> applyCreatedBlock pske blk
    convertRes createdBlk = (Right createdBlk, headerHash createdBlk)

----------------------------------------------------------------------------
-- MainBlock creation
----------------------------------------------------------------------------

-- | Create a new main block for the given slot on top of our
-- tip. This function assumes that lock on block application is taken
-- (hence 'Internal' suffix). It doesn't apply or verify created
-- block. It only checks whether a block can be created (see
-- 'createMainBlockAndApply') and creates it checks passes.
createMainBlockInternal ::
       forall ssc ctx m. (MonadCreateBlock ssc ctx m)
    => SlotId
    -> ProxySKBlockInfo
    -> m (Either Text (MainBlock ssc))
createMainBlockInternal sId pske = do
    tipHeader <- DB.getTipHeader @ssc
    logInfo $ sformat msgFmt tipHeader
    canCreateBlock sId tipHeader >>= \case
        Left reason -> pure (Left reason)
        Right () -> runExceptT (createMainBlockFinish tipHeader)
  where
    msgFmt = "We are trying to create main block, our tip header is\n"%build
    createMainBlockFinish :: BlockHeader ssc -> ExceptT Text m (MainBlock ssc)
    createMainBlockFinish prevHeader = do
        rawPay <- getRawPayload sId
        sk <- getOurSecretKey
        -- 100 bytes is substracted to account for different unexpected
        -- overhead.  You can see that in bitcoin blocks are 1-2kB less
        -- than limit. So i guess it's fine in general.
        sizeLimit <- (\x -> bool 0 (x - 100) (x > 100)) <$> UDB.getMaxBlockSize
        block <- createMainBlockPure sizeLimit prevHeader pske sId sk rawPay
        logInfo $
            "Created main block of size: " <> sformat memory (biSize block)
        block <$ evaluateNF_ block

canCreateBlock ::
       forall ssc ctx m. MonadCreateBlock ssc ctx m
    => SlotId
    -> BlockHeader ssc
    -> m (Either Text ())
canCreateBlock sId tipHeader =
    runExceptT $ do
        unlessM usCanCreateBlock $
            throwError "this software is obsolete and can't create block"
        unless (EpochOrSlot (Right sId) > tipEOS) $
            throwError "slot id is not greater than one from the tip block"
        unless (tipHeader ^. epochIndexL == siEpoch sId) $
            throwError "we don't know genesis block for this epoch"
        let flatSId = flattenSlotId sId
        -- Small heuristic: let's not check chain quality during the
        -- first quarter of the 0-th epoch, because during this time
        -- weird things can happen (we just launched the system) and
        -- usually we monitor it manually anyway.
        unless (flatSId <= fromIntegral (epochSlots `div` 4)) $ do
            chainQuality <- calcChainQualityM flatSId
            unless (chainQuality >= chainQualityThreshold @Double) $
                throwError $
                sformat ("chain quality is below threshold: "%fixed 3) chainQuality
  where
    tipEOS :: EpochOrSlot
    tipEOS = getEpochOrSlot tipHeader

createMainBlockPure
    :: forall m ssc .
       (MonadError Text m, SscHelpersClass ssc)
    => Byte                   -- ^ Block size limit (real max.value)
    -> BlockHeader ssc
    -> ProxySKBlockInfo
    -> SlotId
    -> SecretKey
    -> RawPayload ssc
    -> m (MainBlock ssc)
createMainBlockPure limit prevHeader pske sId sk rawPayload = do
    bodyLimit <- execStateT computeBodyLimit limit
    body <- createMainBody bodyLimit sId rawPayload
    mkMainBlock (Just prevHeader) sId sk pske body
  where
    -- default ssc to put in case we won't fit a normal one
    defSsc :: SscPayload ssc
    defSsc = sscDefaultPayload @ssc (siSlot sId)
    computeBodyLimit :: StateT Byte m ()
    computeBodyLimit = do
        -- account for block header and serialization overhead, etc;
        let musthaveBody = BC.MainBody emptyTxPayload defSsc def def
        musthaveBlock <-
            mkMainBlock (Just prevHeader) sId sk pske musthaveBody
        let mhbSize = biSize musthaveBlock
        when (mhbSize > limit) $ throwError $
            "Musthave block size is more than limit: " <> show mhbSize
        identity -= biSize musthaveBlock

----------------------------------------------------------------------------
-- MainBlock apply
----------------------------------------------------------------------------

-- This function tries to apply the block we've just created. It also
-- verifies the block before applying it. If the block turns out to be
-- invalid (which should never happen, but it's a precaution) we clear
-- all mempools and try to create a block again. The returned value is
-- the block we applied (usually it's the same as the argument, but
-- can differ if verification fails).
applyCreatedBlock ::
       forall ssc ctx m. (MonadBlockApply ssc ctx m, MonadCreateBlock ssc ctx m)
    => ProxySKBlockInfo
    -> MainBlock ssc
    -> m (MainBlock ssc)
applyCreatedBlock pske createdBlock = applyCreatedBlockDo False createdBlock
  where
    slotId = createdBlock ^. BC.mainBlockSlot
    applyCreatedBlockDo :: Bool -> MainBlock ssc -> m (MainBlock ssc)
    applyCreatedBlockDo isFallback blockToApply =
        verifyBlocksPrefix (one (Right blockToApply)) >>= \case
            Left reason
                | isFallback -> onFailedFallback reason
                | otherwise -> fallback reason
            Right (undos, pollModifier) -> do
                let undo = undos ^. _Wrapped . _neHead
                applyBlocksUnsafe
                    (one (Right blockToApply, undo))
                    (Just pollModifier)
                normalizeMempool
                pure blockToApply
    clearMempools :: m ()
    clearMempools = do
        clearTxpMemPool "fallback@applyCreatedBlock"
        sscResetLocal
        clearUSMemPool
        clearDlgMemPool
    fallback :: Text -> m (MainBlock ssc)
    fallback reason = do
        let message = sformat ("We've created bad main block: "%stext) reason
        logError message
        -- FIXME [CSL-1340]: it should be reported as 'RError'.
        reportMisbehaviourSilent False message
        logDebug $ "Clearing mempools"
        clearMempools
        logDebug $ "Creating empty block"
        createMainBlockInternal slotId pske >>= \case
            Left err ->
                assertionFailed $
                sformat ("Couldn't create a block in fallback: "%stext) err
            Right mainBlock -> applyCreatedBlockDo True mainBlock
    onFailedFallback =
        assertionFailed .
        sformat
            ("We've created bad main block even with empty payload: "%stext)

----------------------------------------------------------------------------
-- MainBody, payload
----------------------------------------------------------------------------

data RawPayload ssc = RawPayload
    { rpTxp    :: ![TxAux]
    , rpSsc    :: !(SscPayload ssc)
    , rpDlg    :: !DlgPayload
    , rpUpdate :: !UpdatePayload
    }

getRawPayload
    :: forall ssc ctx m.
       (MonadCreateBlock ssc ctx m)
    => SlotId
    -> ExceptT Text m (RawPayload ssc)
getRawPayload slotId = do
    localTxs <- lift getLocalTxs
    sortedTxs <- maybe onBrokenTopo pure $ topsortTxs convertTx localTxs
    sscData <- sscGetLocalPayload @ssc slotId
    usPayload <- note onNoUS =<< lift (usPreparePayload slotId)
    dlgPayload <- lift getDlgMempool
    let rawPayload =
            RawPayload
            { rpTxp = map snd sortedTxs
            , rpSsc = sscData
            , rpDlg = dlgPayload
            , rpUpdate = usPayload
            }
    return rawPayload
  where
    convertTx (txId, txAux) = WithHash (taTx txAux) txId
    onBrokenTopo = throwError "Topology of local transactions is broken!"
    onNoUS = "can't obtain US payload to create block"

-- Main purpose of this function is to create main block's body taking
-- limit into account. Usually this function doesn't fail, but we
-- perform some sanity checks just in case.
--
-- Given limit applies only to body, not to other data from block.
createMainBody
    :: forall m ssc .
       (MonadError Text m, SscHelpersClass ssc)
    => Byte  -- ^ Body limit
    -> SlotId
    -> RawPayload ssc
    -> m $ Body $ MainBlockchain ssc
createMainBody bodyLimit sId payload =
    flip evalStateT bodyLimit $ do
        let defSsc :: SscPayload ssc
            defSsc = sscDefaultPayload @ssc (siSlot sId)
        -- include ssc data limited with max half of block space if it's possible
        sscPayload <- ifM (uses identity (<= biSize defSsc)) (pure defSsc) $ do
            halfLeft <- uses identity (`div` 2)
            -- halfLeft > 0, otherwize sscStripPayload may fail
            let sscPayload = sscStripPayload @ssc halfLeft sscData
            flip (maybe $ pure defSsc) sscPayload $ \sscP -> do
                -- we subtract size of empty map because it's
                -- already included in musthaveBlock
                identity -= (biSize sscP - biSize defSsc)
                pure sscP

        -- include delegation certificates and US payload
        let prioritizeUS = even (flattenSlotId sId)
        let psks = getDlgPayload dlgPay
        (psks', usPayload') <-
            if prioritizeUS then do
                usPayload' <- includeUSPayload
                psks' <- takeSome psks
                return (psks', usPayload')
            else do
                psks' <- takeSome psks
                usPayload' <- includeUSPayload
                return (psks', usPayload')
        let dlgPay' = leftToPanic "createMainBlockPure: " $ mkDlgPayload psks'
        -- include transactions
        txs' <- takeSome txs
        -- return the resulting block
        txPayload <- either throwError pure $ mkTxPayload txs'
        let body = BC.MainBody txPayload sscPayload dlgPay' usPayload'
        return body
  where
    RawPayload { rpTxp = txs
               , rpSsc = sscData
               , rpDlg = dlgPay
               , rpUpdate = usPayload
               } = payload
    -- take from a list until the limit is exhausted or the list ends
    takeSome lst = do
        let go lim [] = (lim, [])
            go lim (x:xs) =
                let len = biSize x
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
        let len = biSize usPayload
        if len <= lim
            then (identity -= len) >> return usPayload
            else return def
