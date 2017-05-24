{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block creation logic.

module Pos.Block.Logic.Creation
       ( createGenesisBlock
       , createMainBlock
       , createMainBlockPure
       ) where

import           Universum

import           Control.Lens               (uses, (-=), (.=), _Wrapped)
import           Control.Monad.Catch        (try)
import           Control.Monad.Except       (MonadError (throwError), runExceptT)
import           Data.Default               (Default (def))
import qualified Data.HashMap.Strict        as HM
import qualified Ether
import           Formatting                 (build, ords, sformat, stext, (%))
import           Paths_cardano_sl           (version)
import           Serokell.Data.Memory.Units (Byte)
import           System.Wlog                (logDebug, logError, logInfo)

import           Pos.Binary.Class           (biSize)
import           Pos.Block.Core             (BlockHeader, GenesisBlock, MainBlock,
                                             MainExtraBodyData (..),
                                             MainExtraHeaderData (..), mkGenesisBlock,
                                             mkMainBlock)
import qualified Pos.Block.Core             as BC
import           Pos.Block.Logic.Internal   (applyBlocksUnsafe, toUpdateBlock)
import           Pos.Block.Logic.Util       (withBlkSemaphore)
import           Pos.Block.Logic.VAR        (verifyBlocksPrefix)
import           Pos.Block.Types            (Undo (..))
import           Pos.Constants              (curSoftwareVersion, lastKnownBlockVersion,
                                             slotSecurityParam)
import           Pos.Context                (lrcActionOnEpochReason, npSecretKey)
import           Pos.Core                   (EpochIndex, EpochOrSlot (..), HeaderHash,
                                             ProxySKEither, ProxySKHeavy, SlotId (..),
                                             SlotLeaders, crucialSlot, epochOrSlot,
                                             flattenSlotId, getEpochOrSlot, getSlotIndex,
                                             headerHash, mkLocalSlotIndex)
import           Pos.Crypto                 (SecretKey, WithHash (WithHash))
import           Pos.Data.Attributes        (mkAttributes)
import           Pos.DB                     (DBError (..))
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import           Pos.Delegation.Logic       (clearDlgMemPool, getProxyMempool)
import           Pos.Exception              (assertionFailed, reportFatalError)
import qualified Pos.Lrc.DB                 as LrcDB
import           Pos.Lrc.Error              (LrcError (..))
import           Pos.Reporting              (reportMisbehaviourMasked, reportingFatal)
import           Pos.Ssc.Class              (Ssc (..), SscHelpersClass (sscDefaultPayload, sscStripPayload))
import           Pos.Ssc.Extra              (sscGetLocalPayload, sscResetLocal)
import           Pos.Txp.Core               (TxAux (..), mkTxPayload, topsortTxs)
import           Pos.Txp.MemState           (clearTxpMemPool, getLocalTxsNUndo)
import           Pos.Update.Core            (UpdatePayload (..))
import qualified Pos.Update.DB              as UDB
import           Pos.Update.Logic           (clearUSMemPool, usCanCreateBlock,
                                             usPreparePayload, usVerifyBlocks)
import           Pos.Update.Poll            (PollModifier)
import           Pos.Util                   (maybeThrow, _neHead)
import           Pos.WorkMode.Class         (WorkMode)

----------------------------------------------------------------------------
-- GenesisBlock creation
----------------------------------------------------------------------------

-- | Create genesis block if necessary.
--
-- We create genesis block for current epoch when head of currently
-- known best chain is a 'MainBlock' corresponding to one of last
-- `slotSecurityParam` slots of (i - 1)-th epoch. Main check is that
-- given 'epoch' is `(last stored epoch + 1)`, but we also don't want to
-- create genesis block on top of blocks from previous epoch which are
-- not from last slotSecurityParam slots, because it's practically
-- impossible for them to be valid.
-- [CSL-481] We can consider doing it though.
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
    epochOrSlot (const False) doCheck headEpochOrSlot
  where
    doCheck SlotId {siSlot = slot, ..} =
        siEpoch == epoch - 1 && slot > siSlot (crucialSlot epoch)

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
    tipHeader <- maybeThrow (DBMalformed noHeaderMsg) =<< DB.blkGetHeader tip
    logDebug $ sformat msgTryingFmt epoch tipHeader
    createGenesisBlockFinally tipHeader
  where
    createGenesisBlockFinally tipHeader
        | shouldCreateGenesisBlock epoch (getEpochOrSlot tipHeader) = do
            let blk = mkGenesisBlock (Just tipHeader) epoch leaders
            let newTip = headerHash blk
            runExceptT (usVerifyBlocks False (one (toUpdateBlock (Left blk)))) >>= \case
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
       (WorkMode ssc m)
    => SlotId
    -> Maybe ProxySKEither
    -> m (Either Text (MainBlock ssc))
createMainBlock sId pSk =
    reportingFatal version $ withBlkSemaphore createMainBlockDo
  where
    msgFmt = "We are trying to create main block, our tip header is\n"%build
    createMainBlockDo tip = do
        tipHeader <- DB.getTipHeader
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
    | (EpochOrSlot $ Right sId) <= headSlot =
        Just "slot id is not greater than one from the tip block"
    | otherwise = Nothing
  where
    headSlot = getEpochOrSlot tipHeader
    addSafe si =
        si
        { siSlot =
              either (const maxBound) identity $
              mkLocalSlotIndex (getSlotIndex (siSlot si) + slotSecurityParam)
        }
    maxSlotId = addSafe $ epochOrSlot (`SlotId` minBound) identity headSlot

-- Create main block and apply it, if block passed checks,
-- otherwise clear mem pools and try again.
-- Returns valid block or fail.
-- Here we assume that blkSemaphore has been taken.
createMainBlockFinish
    :: forall ssc m.
       (WorkMode ssc m)
    => SlotId
    -> Maybe ProxySKEither
    -> BlockHeader ssc
    -> ExceptT Text m (MainBlock ssc)
createMainBlockFinish slotId pSk prevHeader = do
    unchecked@(uncheckedBlock, _, _) <- createBlundFromMemPool
    (block, undo, pModifier) <-
        verifyCreatedBlock uncheckedBlock >>=
        either fallbackCreateBlock (const $ pure unchecked)
    logDebug "Created main block/undos, applying"
    lift $ block <$ applyBlocksUnsafe (one (Right block, undo)) (Just pModifier)
  where
    createBlundFromMemPool :: ExceptT Text m (MainBlock ssc, Undo, PollModifier)
    createBlundFromMemPool = do
        -- Get MemPool
        (localTxs, txUndo) <- getLocalTxsNUndo
        sscData <- sscGetLocalPayload @ssc slotId
        usPayload <- note onNoUS =<< lift (usPreparePayload slotId)
        (localPSKs, pskUndo) <- lift getProxyMempool
        -- Create block
        let convertTx (txId, txAux) = WithHash (taTx txAux) txId
        sortedTxs <- maybe onBrokenTopo pure $ topsortTxs convertTx localTxs
        sk <- Ether.asks' npSecretKey
        -- 100 bytes is substracted to account for different unexpected
        -- overhead.  You can see that in bitcoin blocks are 1-2kB less
        -- than limit. So i guess it's fine in general.
        sizeLimit <- (\x -> bool 0 (x - 100) (x > 100)) <$> UDB.getMaxBlockSize
        block <- createMainBlockPure
            sizeLimit prevHeader (map snd sortedTxs) pSk
            slotId localPSKs sscData usPayload sk
        lift $ logInfo $ "Created main block of size: " <> show (biSize block)
        -- Create undo
        (pModifier, verUndo) <-
            runExceptT (usVerifyBlocks False (one (toUpdateBlock (Right block)))) >>=
            either (const $ throwError "Couldn't get pModifier while creating MainBlock") pure
        undo <- Undo <$> (reverse <$> foldM (prependToUndo txUndo) [] sortedTxs)
                     <*> pure pskUndo
                     <*> pure (verUndo ^. _Wrapped . _neHead)
        () <- (undo `deepseq` block) `deepseq` pure ()
        pure (block, undo, pModifier)
    onBrokenTopo = throwError "Topology of local transactions is broken!"
    onAbsentUndo = throwError "Undo for tx from local transactions not found"
    onNoUS = "can't obtain US payload to create block"
    prependToUndo txUndo undos tx =
        case (:undos) <$> HM.lookup (fst tx) txUndo of
            Just res -> pure res
            Nothing  -> onAbsentUndo
    verifyCreatedBlock :: MainBlock ssc -> ExceptT Text m (Either Text ())
    verifyCreatedBlock block = lift $ void <$> verifyBlocksPrefix (one (Right block))
    clearMempools = do
        clearTxpMemPool
        sscResetLocal
        clearUSMemPool
        clearDlgMemPool
    fallbackCreateBlock :: Text -> ExceptT Text m (MainBlock ssc, Undo, PollModifier)
    fallbackCreateBlock er = do
        logError $ sformat ("We've created bad main block: "%stext) er
        lift $ reportMisbehaviourMasked version $
            sformat ("We've created bad main block: "%build) er
        logDebug $ "Creating empty block"
        clearMempools
        emptyBlund@(emptyBlock, _, _) <- createBlundFromMemPool
        verifyCreatedBlock emptyBlock >>=
            either (assertionFailed . sformat ("We couldn't create even block with empty payload: "%stext))
            (const $ pure emptyBlund)

createMainBlockPure
    :: forall m ssc .
       (MonadError Text m, SscHelpersClass ssc)
    => Byte                   -- ^ Block size limit (real max.value)
    -> BlockHeader ssc
    -> [TxAux]
    -> Maybe ProxySKEither
    -> SlotId
    -> [ProxySKHeavy]
    -> SscPayload ssc
    -> UpdatePayload
    -> SecretKey
    -> m (MainBlock ssc)
createMainBlockPure limit prevHeader txs pSk sId psks sscData usPayload sk =
    flip evalStateT limit $ do
        -- default ssc to put in case we won't fit a normal one
        let defSsc = (sscDefaultPayload @ssc (siSlot sId) :: SscPayload ssc)

        -- account for block header and serialization overhead, etc;
        let musthaveBody = BC.MainBody
                (fromMaybe (error "createMainBlockPure: impossible") $ mkTxPayload mempty)
                defSsc [] def
        musthaveBlock <-
            mkMainBlock (Just prevHeader) sId sk pSk musthaveBody extraH extraB
        let mhbSize = biSize musthaveBlock
        when (mhbSize > limit) $ throwError $
            "Musthave block size is more than limit: " <> show mhbSize
        identity -= biSize musthaveBlock

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
        txs' <- takeSome txs
        -- return the resulting block
        txPayload <- either throwError pure $ mkTxPayload txs'
        let body = BC.MainBody txPayload sscPayload psks' usPayload'
        mkMainBlock (Just prevHeader) sId sk pSk body extraH extraB
  where
    -- take from a list until the limit is exhausted or the list ends
    takeSome lst = do
        let go lim [] = (lim, [])
            go lim (x:xs) =
                let len = fromIntegral $ biSize x
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
        let len = fromIntegral $ biSize usPayload
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
