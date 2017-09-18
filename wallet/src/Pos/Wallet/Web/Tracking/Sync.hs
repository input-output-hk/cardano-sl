{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- To support actual wallet accounts we listen to applications and rollbacks
-- of blocks, extract transactions from block and extract our
-- accounts (such accounts which we can decrypt).
-- We synchronise wallet-db (acidic-state) with node-db
-- and support last seen tip for each walletset.
-- There are severals cases when we must  synchronise wallet-db and node-db:
-- • When we relaunch wallet. Desynchronization can be caused by interruption
--   during blocks application/rollback at the previous launch,
--   then wallet-db can fall behind from node-db (when interruption during rollback)
--   or vice versa (when interruption during application)
--   @syncWSetsWithGStateLock@ implements this functionality.
-- • When a user wants to import a secret key. Then we must rely on
--   Utxo (GStateDB), because blockchain can be large.

module Pos.Wallet.Web.Tracking.Sync
       ( syncWalletsWithGState
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWallet
       , rollbackModifierFromWallet
       , BlockLockMode

       , syncWalletOnImport
       , txMempoolToModifier

       , fixingCachedAccModifier
       , fixCachedAccModifierFor
       ) where

import           Universum
import           Unsafe                           (unsafeLast)

import           Control.Lens                     (to)
import           Control.Monad.Catch              (handleAll)
import qualified Data.DList                       as DL
import qualified Data.HashMap.Strict              as HM
import           Data.List                        ((!!))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as M
import           Ether.Internal                   (HasLens (..))
import           Formatting                       (build, sformat, (%))
import           Serokell.Util                    (enumerate)
import           System.Wlog                      (HasLoggerName, WithLogger, logError,
                                                   logInfo, logWarning, modifyLoggerName)

import           Pos.Block.Core                   (BlockHeader, getBlockHeader,
                                                   mainBlockTxPayload)
import           Pos.Block.Types                  (Blund, undoTx)
import           Pos.Client.Txp.History           (TxHistoryEntry (..))
import           Pos.Core                         (Address (..), BlockHeaderStub,
                                                   ChainDifficulty, HasConfiguration,
                                                   HasDifficulty (..), HeaderHash,
                                                   Timestamp, aaPkDerivationPath,
                                                   addrAttributesUnwrapped,
                                                   blkSecurityParam, genesisHash,
                                                   headerHash, headerSlotL,
                                                   makeRootPubKeyAddress,
                                                   timestampToPosix)
import           Pos.Crypto                       (EncryptedSecretKey, HDPassphrase,
                                                   WithHash (..), deriveHDPassphrase,
                                                   encToPublic, hash, shortHashF,
                                                   unpackHDAddressAttr)
import qualified Pos.DB.Block                     as DB
import qualified Pos.DB.DB                        as DB
import           Pos.DB.Rocks                     (MonadRealDB)
import qualified Pos.GState                       as GS
import           Pos.GState.BlockExtra            (foldlUpWhileM, resolveForwardLink)
import           Pos.Slotting                     (MonadSlots (..), MonadSlotsData,
                                                   getSlotStartPure, getSystemStartM)
import           Pos.StateLock                    (Priority (..), StateLock,
                                                   withStateLockNoMetrics)
import           Pos.Txp                          (GenesisUtxo (..), Tx (..), TxAux (..),
                                                   TxId, TxIn (..), TxOutAux (..), TxUndo,
                                                   flattenTxPayload, genesisUtxo, toaOut,
                                                   topsortTxs, txOutAddress)
import           Pos.Txp.MemState.Class           (MonadTxpMem, getLocalTxsNUndo)
import           Pos.Util.Chrono                  (getNewestFirst)
import qualified Pos.Util.Modifier                as MM

import           Pos.Ssc.Class                    (SscHelpersClass)
import           Pos.Util.Servant                 (encodeCType)
import           Pos.Wallet.SscType               (WalletSscType)
import           Pos.Wallet.Web.Account           (MonadKeySearch (..))
import           Pos.Wallet.Web.ClientTypes       (Addr, CId, CWAddressMeta (..), Wal,
                                                   addressToCId, ctmDate, encToCId,
                                                   isTxLocalAddress)
import           Pos.Wallet.Web.Error.Types       (WalletError (..))
import           Pos.Wallet.Web.Pending.Types     (PtxBlockInfo, PtxCondition (PtxApplying, PtxInNewestBlocks))
import           Pos.Wallet.Web.State             (AddressLookupMode (..),
                                                   CustomAddressType (..), WalletTip (..),
                                                   WebWalletModeDB)
import qualified Pos.Wallet.Web.State             as WS
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..), CachedCAccModifier,
                                                   deleteAndInsertIMM, deleteAndInsertMM,
                                                   deleteAndInsertVM, indexedDeletions,
                                                   sortedInsertions)
import           Pos.Wallet.Web.Util              (getWalletAddrMetas)


type BlockLockMode ssc ctx m =
     ( WithLogger m
     , MonadReader ctx m
     , HasLens StateLock ctx StateLock
     , MonadRealDB ctx m
     , DB.MonadBlockDB ssc m
     , MonadMask m
     )

type WalletTrackingEnv ext ctx m =
     ( BlockLockMode WalletSscType ctx m
     , WebWalletModeDB ctx m
     , MonadTxpMem ext ctx m
     , WS.MonadWalletWebDB ctx m
     , MonadSlotsData ctx m
     , WithLogger m
     , HasConfiguration
     )

syncWalletOnImport :: WalletTrackingEnv ext ctx m => EncryptedSecretKey -> m ()
syncWalletOnImport = syncWalletsWithGState @WalletSscType . one

txMempoolToModifier :: WalletTrackingEnv ext ctx m => EncryptedSecretKey -> m CAccModifier
txMempoolToModifier encSK = do
    let wHash (i, TxAux {..}, _) = WithHash taTx i
        wId = encToCId encSK
        getDiff       = const Nothing  -- no difficulty (mempool txs)
        getTs         = const Nothing  -- don't give any timestamp
        getPtxBlkInfo = const Nothing  -- no slot of containing block
    (txs, undoMap) <- getLocalTxsNUndo

    txsWUndo <- forM txs $ \(id, tx) -> case HM.lookup id undoMap of
        Just undo -> pure (id, tx, undo)
        Nothing -> do
            let errMsg = sformat ("There is no undo corresponding to TxId #"%build%" from txp mempool") id
            logError errMsg
            throwM $ InternalError errMsg

    tipH <- DB.getTipHeader @WalletSscType
    allAddresses <- getWalletAddrMetas Ever wId
    case topsortTxs wHash txsWUndo of
        Nothing -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
        Just ordered -> pure $
            trackingApplyTxs @WalletSscType encSK allAddresses getDiff getTs getPtxBlkInfo $
            map (\(_, tx, undo) -> (tx, undo, tipH)) ordered

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

-- Iterate over blocks (using forward links) and actualize our accounts.
syncWalletsWithGState
    :: forall ssc ctx m.
    ( WebWalletModeDB ctx m
    , BlockLockMode ssc ctx m
    , MonadSlotsData ctx m
    , HasConfiguration
    )
    => [EncryptedSecretKey] -> m ()
syncWalletsWithGState encSKs = forM_ encSKs $ \encSK -> handleAll (onErr encSK) $ do
    let wAddr = encToCId encSK
    WS.getWalletSyncTip wAddr >>= \case
        Nothing                -> logWarning $ sformat ("There is no syncTip corresponding to wallet #"%build) wAddr
        Just NotSynced         -> syncDo encSK Nothing
        Just (SyncedWith wTip) -> DB.blkGetHeader wTip >>= \case
            Nothing ->
                throwM $ InternalError $
                    sformat ("Couldn't get block header of wallet "%build
                                %" by last synced hh: "%build) wAddr wTip
            Just wHeader -> syncDo encSK (Just wHeader)
  where
    onErr encSK = logWarning . sformat fmt (encToCId encSK)
    fmt = "Sync of wallet "%build%" failed: "%build
    syncDo :: EncryptedSecretKey -> Maybe (BlockHeader ssc) -> m ()
    syncDo encSK wTipH = do
        let wdiff = maybe (0::Word32) (fromIntegral . ( ^. difficultyL)) wTipH
        gstateTipH <- DB.getTipHeader @ssc
        -- If account's syncTip is before the current gstate's tip,
        -- then it loads accounts and addresses starting with @wHeader@.
        -- syncTip can be before gstate's the current tip
        -- when we call @syncWalletSetWithTip@ at the first time
        -- or if the application was interrupted during rollback.
        -- We don't load all blocks explicitly, because blockain can be long.
        wNewTip <-
            if (gstateTipH ^. difficultyL > fromIntegral blkSecurityParam + fromIntegral wdiff) then do
                -- Wallet tip is "far" from gState tip,
                -- rollback can't occur more then @blkSecurityParam@ blocks,
                -- so we can sync wallet and GState without the block lock
                -- to avoid blocking of blocks verification/application.
                bh <- unsafeLast . getNewestFirst <$> DB.loadHeadersByDepth (blkSecurityParam + 1) (headerHash gstateTipH)
                logInfo $
                    sformat ("Wallet's tip is far from GState tip. Syncing with "%build%" without the block lock")
                    (headerHash bh)
                syncWalletWithGStateUnsafe encSK wTipH bh
                pure $ Just bh
            else pure wTipH
        withStateLockNoMetrics HighPriority $ \tip -> do
            logInfo $ sformat ("Syncing wallet with "%build%" under the block lock") tip
            tipH <- maybe (error "No block header corresponding to tip") pure =<< DB.blkGetHeader tip
            syncWalletWithGStateUnsafe encSK wNewTip tipH

----------------------------------------------------------------------------
-- Unsafe operations. Core logic.
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take the block lock.

-- BE CAREFUL! This function iterates over blockchain, the blockchain can be large.
syncWalletWithGStateUnsafe
    :: forall ssc ctx m .
    ( WebWalletModeDB ctx m
    , DB.MonadBlockDB ssc m
    , WithLogger m
    , MonadSlotsData ctx m
    , HasConfiguration
    )
    => EncryptedSecretKey      -- ^ Secret key for decoding our addresses
    -> Maybe (BlockHeader ssc) -- ^ Block header corresponding to wallet's tip.
                               --   Nothing when wallet's tip is genesisHash
    -> BlockHeader ssc         -- ^ GState header hash
    -> m ()
syncWalletWithGStateUnsafe encSK wTipHeader gstateH = setLogger $ do
    systemStart  <- getSystemStartM
    slottingData <- GS.getSlottingData

    let gstateHHash = headerHash gstateH
        loadCond (b, _) _ = b ^. difficultyL <= gstateH ^. difficultyL
        wAddr = encToCId encSK
        mappendR r mm = pure (r <> mm)
        diff = (^. difficultyL)
        mDiff = Just . diff
        gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

        mainBlkHeaderTs mBlkH =
          getSlotStartPure systemStart (mBlkH ^. headerSlotL) slottingData
        blkHeaderTs = either (const Nothing) mainBlkHeaderTs

        -- assuming that transactions are not created until syncing is complete
        ptxBlkInfo = const Nothing

        rollbackBlock :: [CWAddressMeta] -> Blund ssc -> CAccModifier
        rollbackBlock allAddresses (b, u) =
            trackingRollbackTxs encSK allAddresses mDiff blkHeaderTs $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)

        applyBlock :: [CWAddressMeta] -> Blund ssc -> m CAccModifier
        applyBlock allAddresses (b, u) = pure $
            trackingApplyTxs encSK allAddresses mDiff blkHeaderTs ptxBlkInfo $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)

        computeAccModifier :: BlockHeader ssc -> m CAccModifier
        computeAccModifier wHeader = do
            allAddresses <- getWalletAddrMetas Ever wAddr
            logInfo $
                sformat ("Wallet "%build%" header: "%build%", current tip header: "%build)
                wAddr wHeader gstateH
            if | diff gstateH > diff wHeader -> do
                     -- If wallet's syncTip is before than the current tip in the blockchain,
                     -- then it loads wallets starting with @wHeader@.
                     -- Sync tip can be before the current tip
                     -- when we call @syncWalletSetWithTip@ at the first time
                     -- or if the application was interrupted during rollback.
                     -- We don't load blocks explicitly, because blockain can be long.
                     maybe (pure mempty)
                         (\wNextH ->
                            foldlUpWhileM (applyBlock allAddresses) wNextH loadCond mappendR mempty)
                         =<< resolveForwardLink wHeader
               | diff gstateH < diff wHeader -> do
                     -- This rollback can occur
                     -- if the application was interrupted during blocks application.
                     blunds <- getNewestFirst <$>
                         DB.loadBlundsWhile (\b -> getBlockHeader b /= gstateH) (headerHash wHeader)
                     pure $ foldl' (\r b -> r <> rollbackBlock allAddresses b) mempty blunds
               | otherwise -> mempty <$ logInfo (sformat ("Wallet "%build%" is already synced") wAddr)

    whenNothing_ wTipHeader $ do
        let encInfo = getEncInfo encSK
            ownGenesisData =
                selectOwnAccounts encInfo (txOutAddress . toaOut . snd) $
                M.toList $ unGenesisUtxo genesisUtxo
            ownGenesisUtxo = M.fromList $ map fst ownGenesisData
            ownGenesisAddrs = map snd ownGenesisData
        mapM_ WS.addWAddress ownGenesisAddrs
        WS.getWalletUtxo >>= WS.setWalletUtxo . (ownGenesisUtxo <>)

    startFromH <- maybe firstGenesisHeader pure wTipHeader
    mapModifier@CAccModifier{..} <- computeAccModifier startFromH
    applyModifierToWallet wAddr gstateHHash mapModifier
    -- Mark the wallet as ready, so it will be available from api endpoints.
    WS.setWalletReady wAddr True
    logInfo $ sformat ("Wallet "%build%" has been synced with tip "
                    %shortHashF%", "%build)
                wAddr (maybe genesisHash headerHash wTipHeader) mapModifier
  where
    firstGenesisHeader :: m (BlockHeader ssc)
    firstGenesisHeader = resolveForwardLink (genesisHash @BlockHeaderStub) >>=
        maybe (error "Unexpected state: genesisHash doesn't have forward link")
            (maybe (error "No genesis block corresponding to header hash") pure <=< DB.blkGetHeader)

-- TODO: @pva701: maybe it would be needed, dunno
-- runWithWalletUtxo
--     :: (MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo, WebWalletModeDB ctx m)
--     => ToilT () (DBToil m) a
--     -> m a
-- runWithWalletUtxo action = do
--     walletUtxo <- WS.getWalletUtxo
--     runDBToil $ fst <$> runToilTLocal (fromUtxo walletUtxo) def mempty action

-- Process transactions on block application,
-- decrypt our addresses, and add/delete them to/from wallet-db.
-- Addresses are used in TxIn's will be deleted,
-- in TxOut's will be added.
trackingApplyTxs
    :: forall ssc . (HasConfiguration, SscHelpersClass ssc)
    => EncryptedSecretKey                          -- ^ Wallet's secret key
    -> [CWAddressMeta]                             -- ^ All addresses in wallet
    -> (BlockHeader ssc -> Maybe ChainDifficulty)  -- ^ Function to determine tx chain difficulty
    -> (BlockHeader ssc -> Maybe Timestamp)        -- ^ Function to determine tx timestamp in history
    -> (BlockHeader ssc -> Maybe PtxBlockInfo)     -- ^ Function to determine pending tx's block info
    -> [(TxAux, TxUndo, BlockHeader ssc)]          -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingApplyTxs (getEncInfo -> encInfo) allAddresses getDiff getTs getPtxBlkInfo txs =
    foldl' applyTx mempty txs
  where
    toTxInOut txid (idx, out) = (TxInUtxo txid idx, TxOutAux out)

    applyTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader ssc) -> CAccModifier
    applyTx CAccModifier{..} (TxAux {..}, undo, blkHeader) =
        let hh = headerHash blkHeader
            mDiff = getDiff blkHeader
            mTs = getTs blkHeader
            hhs = repeat hh
            tx@(UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = taTx
            !txId = hash tx
            -- TODO should we do something with unknown inputs?
            resolvedInputs = catMaybes $ zipWith (fmap . (,)) inps (NE.toList undo)
            txOutgoings = map txOutAddress outs
            txInputs = map (toaOut . snd) resolvedInputs

            ownInputs = selectOwnAccounts encInfo (txOutAddress . toaOut . snd) resolvedInputs
            ownOutputs = selectOwnAccounts encInfo (txOutAddress . snd) $
                enumerate outs
            ownInpAddrMetas = map snd ownInputs
            ownOutAddrMetas = map snd ownOutputs
            ownTxIns = map (fst . fst) ownInputs
            ownTxOuts = map (toTxInOut txId . fst) ownOutputs

            addedHistory =
                if (not $ null ownOutputs) || (not $ null ownInputs)
                then DL.cons (THEntry txId tx mDiff txInputs txOutgoings mTs)
                     camAddedHistory
                else camAddedHistory

            usedAddrs = map cwamId ownOutAddrMetas
            changeAddrs = evalChange allAddresses (map cwamId ownInpAddrMetas) usedAddrs

            mPtxBlkInfo = getPtxBlkInfo blkHeader
            addedPtxCandidates =
                if | Just ptxBlkInfo <- mPtxBlkInfo
                     -> DL.cons (txId, ptxBlkInfo) camAddedPtxCandidates
                   | otherwise
                     -> camAddedPtxCandidates
        in CAccModifier
            (deleteAndInsertIMM [] ownOutAddrMetas camAddresses)
            (deleteAndInsertVM [] (zip usedAddrs hhs) camUsed)
            (deleteAndInsertVM [] (zip changeAddrs hhs) camChange)
            (deleteAndInsertMM ownTxIns ownTxOuts camUtxo)
            addedHistory
            camDeletedHistory
            addedPtxCandidates
            camDeletedPtxCandidates

-- Process transactions on block rollback.
-- Like @trackingApplyTx@, but vise versa.
trackingRollbackTxs
    :: forall ssc . (HasConfiguration, SscHelpersClass ssc)
    => EncryptedSecretKey -- ^ Wallet's secret key
    -> [CWAddressMeta] -- ^ All adresses
    -> (BlockHeader ssc -> Maybe ChainDifficulty)  -- ^ Function to determine tx chain difficulty
    -> (BlockHeader ssc -> Maybe Timestamp)        -- ^ Function to determine tx timestamp in history
    -> [(TxAux, TxUndo, BlockHeader ssc)] -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingRollbackTxs (getEncInfo -> encInfo) allAddress getDiff getTs txs =
    foldl' rollbackTx mempty txs
  where
    rollbackTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader ssc) -> CAccModifier
    rollbackTx CAccModifier{..} (TxAux {..}, NE.toList -> undoL, blkHeader) = do
        let hh = headerHash blkHeader
            hhs = repeat hh
            mDiff = getDiff blkHeader
            mTs = getTs blkHeader
            tx@(UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = taTx
            !txid = hash taTx
            undoL' = catMaybes undoL
            resolvedInputs = zip inps undoL'
            txOutgoings = map txOutAddress outs
            txInputs = map (toaOut . snd) resolvedInputs

            ownInputs = selectOwnAccounts encInfo (txOutAddress . toaOut) undoL'
            ownOutputs = selectOwnAccounts encInfo txOutAddress $ outs
            ownInputMetas = map snd ownInputs
            ownOutputMetas = map snd ownOutputs
            ownInputAddrs = map cwamId ownInputMetas
            ownOutputAddrs = map cwamId ownOutputMetas

            l = fromIntegral (length outs) :: Word32
            ownTxIns = zip inps $ map fst ownInputs
            ownTxOuts = map (TxInUtxo txid) ([0 .. l - 1] :: [Word32])

            th = THEntry txid tx mDiff txInputs txOutgoings mTs

            deletedHistory =
                if (not $ null ownInputAddrs) || (not $ null ownOutputAddrs)
                then DL.snoc camDeletedHistory $ hash taTx
                else camDeletedHistory

            deletedPtxCandidates = DL.cons (txid, th) camDeletedPtxCandidates

        -- Rollback isn't needed, because we don't use @utxoGet@
        -- (undo contains all required information)
        let usedAddrs = map cwamId ownOutputMetas
            changeAddrs = evalChange allAddress ownInputAddrs ownOutputAddrs
        CAccModifier
            (deleteAndInsertIMM ownOutputMetas [] camAddresses)
            (deleteAndInsertVM (zip usedAddrs hhs) [] camUsed)
            (deleteAndInsertVM (zip changeAddrs hhs) [] camChange)
            (deleteAndInsertMM ownTxOuts ownTxIns camUtxo)
            camAddedHistory
            deletedHistory
            camAddedPtxCandidates
            deletedPtxCandidates

applyModifierToWallet
    :: WebWalletModeDB ctx m
    => CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
applyModifierToWallet wid newTip CAccModifier{..} = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ WS.addWAddress (sortedInsertions camAddresses)
    mapM_ (WS.addCustomAddress UsedAddr . fst) (MM.insertions camUsed)
    mapM_ (WS.addCustomAddress ChangeAddr . fst) (MM.insertions camChange)
    WS.getWalletUtxo >>= WS.setWalletUtxo . MM.modifyMap camUtxo
    oldCachedHist <- fromMaybe [] <$> WS.getHistoryCache wid
    sortedAddedHistory <- sortTxs (DL.toList camAddedHistory)
    WS.updateHistoryCache wid $ sortedAddedHistory <> oldCachedHist
    -- resubmitting worker can change ptx in db nonatomically, but
    -- tracker has priority over the resubmiter, thus do not use CAS here
    forM_ camAddedPtxCandidates $ \(txid, ptxBlkInfo) ->
        WS.setPtxCondition wid txid (PtxInNewestBlocks ptxBlkInfo)
    WS.setWalletSyncTip wid newTip
  where
    getTxTime tx = ctmDate <<$>> WS.getTxMeta wid (encodeCType $ _thTxId tx)
    sortTxs txs = do
        txsWTime <- forM txs $ \tx -> (tx, ) <$> getTxTime tx
        let txRealTime (THEntry{..}, mtime) =
                mtime <|> (timestampToPosix <$> _thTimestamp)
        return $ map fst $ sortOn (fmap Down . txRealTime) txsWTime

rollbackModifierFromWallet
    :: (WebWalletModeDB ctx m, MonadSlots ctx m)
    => CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
rollbackModifierFromWallet wid newTip CAccModifier{..} = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ WS.removeWAddress (indexedDeletions camAddresses)
    mapM_ (WS.removeCustomAddress UsedAddr) (MM.deletions camUsed)
    mapM_ (WS.removeCustomAddress ChangeAddr) (MM.deletions camChange)
    WS.getWalletUtxo >>= WS.setWalletUtxo . MM.modifyMap camUtxo
    forM_ camDeletedPtxCandidates $ \(txid, poolInfo) -> do
        curSlot <- getCurrentSlotInaccurate
        WS.ptxUpdateMeta wid txid (WS.PtxResetSubmitTiming curSlot)
        WS.setPtxCondition wid txid (PtxApplying poolInfo)
    WS.getHistoryCache wid >>= \case
        Nothing -> pure ()
        Just oldCachedHist -> do
            WS.updateHistoryCache wid $
                removeFromHead (DL.toList camDeletedHistory) oldCachedHist
    WS.setWalletSyncTip wid newTip
  where
    removeFromHead :: [TxId] -> [TxHistoryEntry] -> [TxHistoryEntry]
    removeFromHead [] ths = ths
    removeFromHead _ [] = []
    removeFromHead (txId : txIds) (THEntry {..} : thes) =
        if txId == _thTxId
        then removeFromHead txIds thes
        else error "rollbackModifierFromWallet: removeFromHead: \
                   \rollbacked tx ID is not present in history cache!"

evalChange
    :: [CWAddressMeta] -- ^ All adresses
    -> [CId Addr]      -- ^ Own input addresses of tx
    -> [CId Addr]      -- ^ Own outputs addresses of tx
    -> [CId Addr]
evalChange allAddresses inputs outputs
    | null inputs || null outputs = []
    | otherwise = filter (isTxLocalAddress allAddresses (NE.fromList inputs)) outputs

getEncInfo :: EncryptedSecretKey -> (HDPassphrase, CId Wal)
getEncInfo encSK = do
    let pubKey = encToPublic encSK
    let hdPass = deriveHDPassphrase pubKey
    let wCId = addressToCId $ makeRootPubKeyAddress pubKey
    (hdPass, wCId)

selectOwnAccounts
    :: (HDPassphrase, CId Wal)
    -> (a -> Address)
    -> [a]
    -> [(a, CWAddressMeta)]
selectOwnAccounts encInfo getAddr =
    mapMaybe (\a -> (a,) <$> decryptAccount encInfo (getAddr a))

setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (<> "wallet" <> "sync")

decryptAccount :: (HDPassphrase, CId Wal) -> Address -> Maybe CWAddressMeta
decryptAccount (hdPass, wCId) addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ CWAddressMeta wCId (derPath !! 0) (derPath !! 1) (addressToCId addr)

----------------------------------------------------------------------------
-- Cached modifier
----------------------------------------------------------------------------

-- | Evaluates `txMempoolToModifier` and provides result as a parameter
-- to given function.
fixingCachedAccModifier
    :: (WalletTrackingEnv ext ctx m, MonadKeySearch key m)
    => (CachedCAccModifier -> key -> m a)
    -> key -> m a
fixingCachedAccModifier action key =
    findKey key >>= txMempoolToModifier >>= flip action key

fixCachedAccModifierFor
    :: (WalletTrackingEnv ext ctx m, MonadKeySearch key m)
    => key
    -> (CachedCAccModifier -> m a)
    -> m a
fixCachedAccModifierFor key action =
    fixingCachedAccModifier (const . action) key
