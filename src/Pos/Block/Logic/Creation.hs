{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Block creation logic.

module Pos.Block.Logic.Creation
       ( createGenesisBlock
       , createMainBlock

       -- * Internals
       , RawPayload (..)
       , createMainBlockPure
       ) where

import           Universum
import           Unsafe                     (unsafeHead)

import           Control.Lens               (uses, (-=), (.=), _Wrapped)
import           Control.Monad.Catch        (try)
import           Control.Monad.Except       (MonadError (throwError), runExceptT)
import           Data.Default               (Default (def))
import qualified Data.HashMap.Strict        as HM
import           Ether.Internal             (HasLens (..))
import           Formatting                 (build, ords, sformat, stext, (%))
import           Paths_cardano_sl           (version)
import           Serokell.Data.Memory.Units (Byte, memory)
import           System.Wlog                (logDebug, logError, logInfo)

import           Pos.Binary.Class           (biSize)
import           Pos.Block.Core             (BlockHeader, GenesisBlock, MainBlock,
                                             MainBlockchain, mkGenesisBlock, mkMainBlock)
import qualified Pos.Block.Core             as BC
import           Pos.Block.Logic.Internal   (BlockApplyMode, BlockVerifyMode,
                                             applyBlocksUnsafe, toUpdateBlock)
import           Pos.Block.Logic.Util       (calcChainQualityM, withBlkSemaphore)
import           Pos.Block.Logic.VAR        (verifyBlocksPrefix)
import           Pos.Block.Slog             (HasSlogContext (..), SlogUndo (..),
                                             slogGetLastSlots)
import           Pos.Block.Types            (Undo (..))
import           Pos.Constants              (blkSecurityParam, slotSecurityParam)
import           Pos.Context                (BlkSemaphore, HasPrimaryKey, getOurSecretKey,
                                             lrcActionOnEpochReason)
import           Pos.Core                   (Blockchain (..), EpochIndex,
                                             EpochOrSlot (..), HeaderHash, LocalSlotIndex,
                                             SlotId (..), SlotLeaders, addLocalSlotIndex,
                                             epochIndexL, epochOrSlot, flattenSlotId,
                                             getEpochOrSlot, headerHash, siSlotL)
import           Pos.Crypto                 (SecretKey, WithHash (WithHash))
import           Pos.DB                     (DBError (..))
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import           Pos.Delegation.Logic       (clearDlgMemPool, getDlgMempool)
import           Pos.Delegation.Types       (DlgPayload (getDlgPayload), ProxySKBlockInfo,
                                             mkDlgPayload)
import           Pos.Exception              (assertionFailed, reportFatalError)
import qualified Pos.Lrc.DB                 as LrcDB
import           Pos.Lrc.Error              (LrcError (..))
import           Pos.Reporting              (reportMisbehaviourSilent, reportingFatal)
import           Pos.Ssc.Class              (Ssc (..), SscHelpersClass (sscDefaultPayload, sscStripPayload))
import           Pos.Ssc.Extra              (sscGetLocalPayload, sscResetLocal)
import           Pos.Txp.Core               (TxAux (..), emptyTxPayload, mkTxPayload,
                                             topsortTxs)
import           Pos.Txp.MemState           (clearTxpMemPool, getLocalTxsNUndo)
import           Pos.Update.Core            (UpdatePayload (..))
import qualified Pos.Update.DB              as UDB
import           Pos.Update.Logic           (clearUSMemPool, usCanCreateBlock,
                                             usPreparePayload, usVerifyBlocks)
import           Pos.Update.Poll            (PollModifier, USUndo)
import           Pos.Util                   (maybeThrow, _neHead)
import           Pos.Util.Chrono            (OldestFirst (..))
import           Pos.Util.Util              (leftToPanic)

type CreationMode ssc ctx m
     = ( BlockApplyMode ssc ctx m
       , MonadReader ctx m
       , HasPrimaryKey ctx
       , HasLens BlkSemaphore ctx BlkSemaphore
       , HasSlogContext ctx
       )

----------------------------------------------------------------------------
-- GenesisBlock creation
----------------------------------------------------------------------------

-- | Create genesis block if necessary.
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
createGenesisBlock
    :: forall ssc ctx m.
       CreationMode ssc ctx m
    => EpochIndex -> m (Maybe (GenesisBlock ssc))
-- Genesis block for 0-th epoch is hardcoded.
createGenesisBlock 0 = pure Nothing
createGenesisBlock epoch = reportingFatal version $ do
    leadersOrErr <-
        try $
        lrcActionOnEpochReason epoch "there are no leaders" LrcDB.getLeaders
    case leadersOrErr of
        Left UnknownBlocksForLrc ->
            Nothing <$ logInfo "createGenesisBlock: not enough blocks for LRC"
        Left err -> throwM err
        Right leaders -> withBlkSemaphore (createGenesisBlockDo epoch leaders)

createGenesisBlockDo
    :: forall ssc ctx m.
       BlockApplyMode ssc ctx m
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
    chainQualityThreshold :: Double
    chainQualityThreshold =
        realToFrac blkSecurityParam / realToFrac slotSecurityParam
    shouldCreate (Left _) = pure False
    -- This is true iff tip is from 'epoch' - 1 and last
    -- 'blkSecurityParam' blocks fully fit into last
    -- 'slotSecurityParam' slots of 'epoch'.
    shouldCreate (Right mb)
        | mb ^. epochIndexL /= epoch - 1 = pure False
        | otherwise =
            (chainQualityThreshold <=) <$>
            calcChainQualityM (flattenSlotId $ SlotId epoch minBound)
    actuallyCreate tipHeader = do
        let blk = mkGenesisBlock (Just tipHeader) epoch leaders
        let newTip = headerHash blk
        verifyBlocksPrefix (one (Left blk)) >>= \case
            Left err -> reportFatalError $ pretty err
            Right (undos, pollModifier) ->
                let undo = undos ^. _Wrapped . _neHead
                in applyBlocksUnsafe (one (Left blk, undo)) (Just pollModifier) $>
                   (Just blk, newTip)
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
    :: forall ssc ctx m.
       (CreationMode ssc ctx m)
    => SlotId
    -> ProxySKBlockInfo
    -> m (Either Text (MainBlock ssc))
createMainBlock sId pske =
    reportingFatal version $ withBlkSemaphore createMainBlockDo
  where
    msgFmt = "We are trying to create main block, our tip header is\n"%build
    createMainBlockDo tip = do
        tipHeader <- DB.getTipHeader @ssc
        logInfo $ sformat msgFmt tipHeader
        canWrtUs <- usCanCreateBlock
        case (canCreateBlock sId tipHeader, canWrtUs) of
            (_, False) ->
                return (Left "this software can't create block", tip)
            (Nothing, True)  -> convertRes tip <$>
                runExceptT (createMainBlockFinish sId pske tipHeader)
            (Just err, True) -> return (Left err, tip)
    convertRes oldTip (Left e) = (Left e, oldTip)
    convertRes _ (Right blk)   = (Right blk, headerHash blk)

canCreateBlock :: SlotId -> BlockHeader ssc -> Maybe Text
canCreateBlock sId tipHeader
    | sId > maxSlotId = Just "slot id is too big, we don't know recent block"
    | (EpochOrSlot $ Right sId) <= headSlot =
        Just "slot id is not greater than one from the tip block"
    | otherwise = Nothing
  where
    headSlot :: EpochOrSlot
    headSlot = getEpochOrSlot tipHeader
    addSafe :: LocalSlotIndex -> LocalSlotIndex
    addSafe = fromMaybe maxBound . addLocalSlotIndex slotSecurityParam
    maxSlotId :: SlotId
    maxSlotId = over siSlotL addSafe $
                epochOrSlot (`SlotId` minBound) identity headSlot

data RawPayload ssc = RawPayload
    { rpTxp    :: ![TxAux]
    , rpSsc    :: !(SscPayload ssc)
    , rpDlg    :: !DlgPayload
    , rpUpdate :: !UpdatePayload
    }

-- Create main block and apply it, if block passed checks,
-- otherwise clear mem pools and try again.
-- Returns valid block or fail.
-- Here we assume that blkSemaphore has been taken.
createMainBlockFinish
    :: forall ssc ctx m.
       (CreationMode ssc ctx m)
    => SlotId
    -> ProxySKBlockInfo
    -> BlockHeader ssc
    -> ExceptT Text m (MainBlock ssc)
createMainBlockFinish slotId pske prevHeader = do
    unchecked@(uncheckedBlock, _, _) <- createBlundFromMemPool
    (block, undo, pModifier) <-
        verifyCreatedBlock uncheckedBlock (pure unchecked) fallbackCreateBlock
    lift $ applyBlocksUnsafe (one (Right block, undo)) (Just pModifier)
    pure block
  where
    createBlundFromMemPool :: ExceptT Text m (MainBlock ssc, Undo, PollModifier)
    createBlundFromMemPool = do
        (rawPay, undoNoUS) <- getRawPayloadAndUndo slotId
        -- Create block
        sk <- getOurSecretKey
        -- 100 bytes is substracted to account for different unexpected
        -- overhead.  You can see that in bitcoin blocks are 1-2kB less
        -- than limit. So i guess it's fine in general.
        sizeLimit <- (\x -> bool 0 (x - 100) (x > 100)) <$> UDB.getMaxBlockSize
        block <- createMainBlockPure sizeLimit prevHeader pske slotId sk rawPay
        logInfo $ "Created main block of size: " <> sformat memory (biSize block)
        -- Create Undo
        (pModifier, usUndo) <-
            runExceptT (usVerifyBlocks False (one (toUpdateBlock (Right block)))) >>=
            either (const $ throwError "Couldn't get pModifier while creating MainBlock") pure
        let undo = undoNoUS (usUndo ^. _Wrapped . _neHead)
        evaluateNF_ (undo, block)
        pure (block, undo, pModifier)
    verifyCreatedBlock ::
        forall n a. BlockVerifyMode ssc ctx n =>
        MainBlock ssc -> n a -> (Text -> n a) -> n a
    verifyCreatedBlock block onSuccess onFailure =
        verifyBlocksPrefix (one (Right block)) >>=
        either onFailure (const onSuccess)
    clearMempools = do
        clearTxpMemPool
        sscResetLocal
        clearUSMemPool
        lift $ clearDlgMemPool
    fallbackCreateBlock :: Text -> ExceptT Text m (MainBlock ssc, Undo, PollModifier)
    fallbackCreateBlock er = do
        logError $ sformat ("We've created bad main block: "%stext) er
        lift $ reportMisbehaviourSilent version $
            sformat ("We've created bad main block: "%build) er
        logDebug $ "Creating empty block"
        clearMempools
        emptyBlund@(emptyBlock, _, _) <- createBlundFromMemPool
        lift $ verifyCreatedBlock emptyBlock (pure emptyBlund)
            (assertionFailed . sformat
             ("We couldn't create even block with empty payload: "%stext))

getRawPayloadAndUndo
    :: forall ssc ctx m.
       (CreationMode ssc ctx m)
    => SlotId
    -> ExceptT Text m (RawPayload ssc, (USUndo -> Undo))
getRawPayloadAndUndo slotId = do
    (localTxs, txUndo) <- getLocalTxsNUndo
    sortedTxs <- maybe onBrokenTopo pure $ topsortTxs convertTx localTxs
    sscData <- sscGetLocalPayload @ssc slotId
    usPayload <- note onNoUS =<< lift (usPreparePayload slotId)
    (dlgPayload, pskUndo) <- lift $ getDlgMempool
    txpUndo <- reverse <$> foldM (prependToUndo txUndo) [] sortedTxs
    slogUndo <- getSlogUndo
    let undo usUndo = Undo txpUndo pskUndo usUndo slogUndo
    let rawPayload =
            RawPayload
            { rpTxp = map snd sortedTxs
            , rpSsc = sscData
            , rpDlg = dlgPayload
            , rpUpdate = usPayload
            }
    return (rawPayload, undo)
  where
    prependToUndo txUndo undos tx =
        case (: undos) <$> HM.lookup (fst tx) txUndo of
            Just res -> pure res
            Nothing  -> onAbsentUndo
    convertTx (txId, txAux) = WithHash (taTx txAux) txId
    onBrokenTopo = throwError "Topology of local transactions is broken!"
    onAbsentUndo = throwError "Undo for tx from local transactions not found"
    onNoUS = "can't obtain US payload to create block"
    getSlogUndo =
        slogGetLastSlots <&> \case
            (OldestFirst lastSlots)
                | length lastSlots < fromIntegral blkSecurityParam ->
                    SlogUndo Nothing
                | otherwise -> SlogUndo $ Just $ unsafeHead lastSlots

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
