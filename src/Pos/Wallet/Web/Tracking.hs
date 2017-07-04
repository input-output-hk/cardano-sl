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

module Pos.Wallet.Web.Tracking
       ( syncWalletsWithGStateLock
       , selectAccountsFromUtxoLock
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWallet
       , rollbackModifierFromWallet
       , BlockLockMode
       , CAccModifier (..)
       , MonadWalletTracking (..)

       , syncWalletsAtStartWebWallet
       , syncOnImportWebWallet
       , txMempoolToModifierWebWallet

       , getWalletAddrMetasDB
       ) where

import           Universum

import           Control.Lens               (to)
import           Control.Monad.Trans        (MonadTrans)
import           Data.DList                 (DList)
import qualified Data.DList                 as DL
import           Data.List                  ((!!))
import qualified Data.List.NonEmpty         as NE
import           Ether.Internal             (HasLens (..))
import           Formatting                 (build, int, sformat, (%))
import           Mockable                   (MonadMockable, SharedAtomicT)
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger, logDebug, logInfo, logWarning)

import           Pos.Block.Core             (BlockHeader, getBlockHeader,
                                             mainBlockTxPayload)
import           Pos.Block.Logic            (withBlkSemaphore, withBlkSemaphore_)
import           Pos.Block.Types            (Blund, undoTx)
import           Pos.Client.Txp.History     (TxHistoryEntry (..))
import           Pos.Constants              (genesisHash)
import           Pos.Context                (BlkSemaphore)
import           Pos.Core                   (AddrPkAttrs (..), Address (..),
                                             ChainDifficulty, HasDifficulty (..),
                                             HeaderHash, Timestamp, headerHash,
                                             headerSlotL, makePubKeyAddress)
import           Pos.Crypto                 (EncryptedSecretKey, HDPassphrase,
                                             WithHash (..), deriveHDPassphrase,
                                             encToPublic, hash, shortHashF,
                                             unpackHDAddressAttr)
import           Pos.Crypto.HDDiscovery     (discoverHDAddresses)
import           Pos.Data.Attributes        (Attributes (..))
import qualified Pos.DB.Block               as DB
import           Pos.DB.Class               (MonadRealDB)
import qualified Pos.DB.DB                  as DB
import           Pos.DB.Error               (DBError (DBMalformed))
import qualified Pos.DB.GState              as GS
import           Pos.DB.GState.BlockExtra   (foldlUpWhileM, resolveForwardLink)
import           Pos.Slotting               (getSlotStartPure)
import           Pos.Txp.Core               (Tx (..), TxAux (..), TxId, TxOutAux (..),
                                             TxUndo, flattenTxPayload, toaOut, topsortTxs,
                                             txOutAddress)
import           Pos.Txp.MemState.Class     (MonadTxpMem, getLocalTxs)
import           Pos.Txp.Toil               (MonadUtxo (..), MonadUtxoRead (..), ToilT,
                                             applyTxToUtxo, evalToilTEmpty, runDBToil)
import           Pos.Util.Chrono            (getNewestFirst)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Util              (maybeThrow)

import           Pos.Ssc.Class              (SscHelpersClass)
import           Pos.Wallet.SscType         (WalletSscType)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CAccountMeta (..), CId,
                                             CWAddressMeta (..), Wal, addrMetaToAccount,
                                             addressToCId, aiWId, encToCId,
                                             isTxLocalAddress)
import           Pos.Wallet.Web.State       (AddressLookupMode (..),
                                             CustomAddressType (..), WebWalletModeDB)
import qualified Pos.Wallet.Web.State       as WS

type VoidModifier a = MM.MapModifier a ()

data CAccModifier = CAccModifier {
      camAddresses      :: !(VoidModifier CWAddressMeta)
    , camUsed           :: !(VoidModifier (CId Addr, HeaderHash))
    , camChange         :: !(VoidModifier (CId Addr, HeaderHash))
    , camAddedHistory   :: !(DList TxHistoryEntry)
    , camDeletedHistory :: !(DList TxId)
    }

instance Monoid CAccModifier where
    mempty = CAccModifier mempty mempty mempty mempty mempty
    (CAccModifier a b c ah dh) `mappend` (CAccModifier a1 b1 c1 ah1 dh1) =
        CAccModifier (a <> a1) (b <> b1) (c <> c1) (ah1 <> ah) (dh <> dh1)

type BlockLockMode ssc ctx m =
    ( WithLogger m
    , MonadReader ctx m
    , HasLens BlkSemaphore ctx BlkSemaphore
    , MonadRealDB ctx m
    , DB.MonadBlockDB ssc m
    , MonadMask m
    )

class Monad m => MonadWalletTracking m where
    syncWalletsAtStart :: [EncryptedSecretKey] -> m ()
    syncOnImport :: EncryptedSecretKey -> m ()
    txMempoolToModifier :: EncryptedSecretKey -> m CAccModifier

instance {-# OVERLAPPABLE #-}
    ( MonadWalletTracking m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        MonadWalletTracking (t m)
  where
    syncWalletsAtStart = lift . syncWalletsAtStart
    syncOnImport = lift . syncOnImport
    txMempoolToModifier = lift . txMempoolToModifier

type WalletTrackingEnv ext ctx m =
     (BlockLockMode WalletSscType ctx m, MonadMockable m, MonadTxpMem ext ctx m, WS.MonadWalletWebDB ctx m)

syncWalletsAtStartWebWallet :: WalletTrackingEnv ext ctx m => [EncryptedSecretKey] -> m ()
syncWalletsAtStartWebWallet = syncWalletsWithGStateLock @WalletSscType

syncOnImportWebWallet :: WalletTrackingEnv ext ctx m => EncryptedSecretKey -> m ()
syncOnImportWebWallet = (() <$) . selectAccountsFromUtxoLock @WalletSscType . one

txMempoolToModifierWebWallet :: WalletTrackingEnv ext ctx m => EncryptedSecretKey -> m CAccModifier
txMempoolToModifierWebWallet encSK = do
    let wHash (i, TxAux {..}) = WithHash taTx i
        wId = encToCId encSK
        getDiff = const Nothing  -- no difficulty (mempool txs)
        getTs = const Nothing  -- don't give any timestamp
    txs <- getLocalTxs
    tipH <- DB.getTipHeader @WalletSscType
    allAddresses <- getWalletAddrMetasDB Ever wId
    case topsortTxs wHash txs of
        Nothing -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
        Just (map snd -> ordered) ->
            runDBToil $
            evalToilTEmpty $
            trackingApplyTxs encSK allAddresses getDiff getTs $
            zip ordered $ repeat tipH

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

-- Select our accounts from Utxo and put to wallet-db.
-- Used for importing of a secret key.
selectAccountsFromUtxoLock
    :: forall ssc ctx m . (WebWalletModeDB ctx m, BlockLockMode ssc ctx m)
    => [EncryptedSecretKey]
    -> m [CWAddressMeta]
selectAccountsFromUtxoLock encSKs = withBlkSemaphore $ \tip -> do
    let (hdPass, wAddr) = unzip $ map getEncInfo encSKs
    logDebug $ sformat ("Select accounts from Utxo: tip "%build%" for "%listJson) tip wAddr
    addresses <- discoverHDAddresses hdPass
    let allAddresses = concatMap createWAddresses $ zip wAddr addresses
    mapM_ addMetaInfo allAddresses
    logDebug (sformat ("After selection from Utxo addresses was added: "%listJson) allAddresses)
    return (allAddresses, tip)
  where
    createWAddresses :: (CId Wal, [(Address, [Word32])]) -> [CWAddressMeta]
    createWAddresses (wAddr, addresses) = do
        let (ads, paths) = unzip addresses
        mapMaybe createWAddress $ zip3 (repeat wAddr) ads paths

    createWAddress :: (CId Wal, Address, [Word32]) -> Maybe CWAddressMeta
    createWAddress (wAddr, addr, derPath) = do
        guard $ length derPath == 2
        pure $ CWAddressMeta wAddr (derPath !! 0) (derPath !! 1) (addressToCId addr)

    addMetaInfo :: CWAddressMeta -> m ()
    addMetaInfo cwMeta = do
        let accId = addrMetaToAccount cwMeta
            accMeta = CAccountMeta
                      { caName = sformat ("Account #"%int) $ aiIndex accId
                      }
        WS.createAccount accId accMeta
        WS.addWAddress cwMeta

-- Iterate over blocks (using forward links) and actualize our accounts.
syncWalletsWithGStateLock
    :: forall ssc ctx m . (WebWalletModeDB ctx m, BlockLockMode ssc ctx m)
    => [EncryptedSecretKey]
    -> m ()
syncWalletsWithGStateLock encSKs = withBlkSemaphore_ $ \tip ->
    tip <$ mapM_ (syncWalletWithGState @ssc) encSKs

----------------------------------------------------------------------------
-- Unsafe operations. Core logic.
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take the block lock.

syncWalletWithGState
    :: forall ssc ctx m .
    ( WebWalletModeDB ctx m
    , MonadRealDB ctx m
    , DB.MonadBlockDB ssc m
    , WithLogger m)
    => EncryptedSecretKey
    -> m ()
syncWalletWithGState encSK = do
    tipH <- DB.getTipHeader @ssc
    slottingData <- GS.getSlottingData

    let wAddr = encToCId encSK
        constTrue = \_ _ -> True
        mappendR r mm = pure (r <> mm)
        diff = (^. difficultyL)
        mDiff = Just . diff
        gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)
        mainBlkHeaderTs mBlkH =
            getSlotStartPure True (mBlkH ^. headerSlotL) slottingData
        blkHeaderTs = either (const Nothing) mainBlkHeaderTs

        rollbackBlock :: [CWAddressMeta] -> Blund ssc -> CAccModifier
        rollbackBlock allAddresses (b, u) =
            trackingRollbackTxs encSK allAddresses $
            zip3 (gbTxs b) (undoTx u) (repeat $ headerHash b)

        applyBlock :: (WithLogger m1, MonadUtxoRead m1)
                   => [CWAddressMeta] -> Blund ssc -> ToilT () m1 CAccModifier
        applyBlock allAddresses (b, _) = trackingApplyTxs encSK allAddresses mDiff blkHeaderTs $
                                         zip (gbTxs b) (repeat $ getBlockHeader b)

        sync :: HeaderHash -> BlockHeader ssc -> m ()
        sync wTip tipH' = DB.blkGetHeader wTip >>= \case
            Nothing ->
                logWarning $
                sformat ("Couldn't get block header of wallet "%build
                         %" by last synced hh: "%build) wAddr wTip
            Just wHeader -> do
                mapModifier@CAccModifier{..} <- compareHeaders wHeader tipH'
                applyModifierToWallet wAddr (headerHash tipH') mapModifier
                logDebug $ sformat ("Wallet "%build
                                    %" has been synced with tip "%shortHashF
                                    %", added addresses: "%listJson
                                    %", deleted addresses: "%listJson
                                    %", used addresses: "%listJson
                                    %", change addresses: "%listJson)
                    wAddr wTip
                    (map fst $ MM.insertions camAddresses)
                    (MM.deletions camAddresses)
                    (map (fst . fst) $ MM.insertions camUsed)
                    (map (fst . fst) $ MM.insertions camChange)

        compareHeaders :: BlockHeader ssc -> BlockHeader ssc -> m CAccModifier
        compareHeaders wHeader tipH' = do
            allAddresses <- getWalletAddrMetasDB Ever wAddr
            logDebug $
                sformat ("Wallet "%build%" header: "%build%", current tip header: "%build)
                wAddr wHeader tipH'
            if | diff tipH' > diff wHeader -> runDBToil $ evalToilTEmpty $ do
                     -- If walletset syncTip before the current tip,
                     -- then it loads wallets starting with @wHeader@.
                     -- Sync tip can be before the current tip
                     -- when we call @syncWalletSetWithTip@ at the first time
                     -- or if the application was interrupted during rollback.
                     -- We don't load blocks explicitly, because blockain can be long.
                     maybe (pure mempty)
                         (\wNextHeader -> foldlUpWhileM
                                          (applyBlock allAddresses)
                                          wNextHeader
                                          constTrue
                                          mappendR
                                          mempty)
                         =<< resolveForwardLink wHeader
               | diff tipH' < diff wHeader -> do
                     -- This rollback can occur
                     -- if the application was interrupted during blocks application.
                     blunds <- getNewestFirst <$>
                         DB.loadBlundsWhile (\b -> getBlockHeader b /= tipH') (headerHash wHeader)
                     pure $ foldl' (\r b -> r <> rollbackBlock allAddresses b) mempty blunds
               | otherwise -> mempty <$ logInfo (sformat ("Wallet "%build%" is already synced") wAddr)

    whenJustM (WS.getWalletSyncTip wAddr) $ \wTip ->
        if | wTip == genesisHash && headerHash tipH == genesisHash ->
                 logDebug $ sformat ("Wallet "%build%" at genesis state, synced") wAddr
           | wTip == genesisHash -> whenJustM (resolveForwardLink wTip) $
                                    \nx -> sync nx tipH
           | otherwise -> sync wTip tipH

-- Process transactions on block application,
-- decrypt our addresses, and add/delete them to/from wallet-db.
-- Addresses are used in TxIn's will be deleted,
-- in TxOut's will be added.
trackingApplyTxs
    :: forall ssc m .
       (MonadUtxo m, SscHelpersClass ssc)
    => EncryptedSecretKey                          -- ^ Wallet's secret key
    -> [CWAddressMeta]                             -- ^ All addresses in wallet
    -> (BlockHeader ssc -> Maybe ChainDifficulty)  -- ^ Function to determine tx chain difficulty
    -> (BlockHeader ssc -> Maybe Timestamp)        -- ^ Function to determine tx timestamp in history
    -> [(TxAux, BlockHeader ssc)]                  -- ^ Txs of blocks and corresponding header hash
    -> m CAccModifier
trackingApplyTxs (getEncInfo -> encInfo) allAddresses getDiff getTs txs =
    foldlM applyTx mempty txs
  where
    applyTx :: CAccModifier -> (TxAux, BlockHeader ssc) -> m CAccModifier
    applyTx CAccModifier{..} (TxAux {..}, blkHeader) = do
        let hh = headerHash blkHeader
            mDiff = getDiff blkHeader
            mTs = getTs blkHeader
            hhs = repeat hh
            tx@(UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = taTx
            txId = hash tx

        resolvedInputs <- map toaOut . catMaybes <$> mapM utxoGet inps
        let txOutgoings = map txOutAddress outs
            txIncomings = map txOutAddress resolvedInputs

            selectOwnAddrMetas = map snd . selectOwnAccounts encInfo identity
            ownInpAddrMetas = selectOwnAddrMetas txIncomings
            ownOutAddrMetas = selectOwnAddrMetas txOutgoings

            addedHistory =
                if (not $ null ownOutAddrMetas) || (not $ null ownOutAddrMetas)
                then DL.cons (THEntry txId tx resolvedInputs mDiff txIncomings txOutgoings mTs)
                     camAddedHistory
                else camAddedHistory

            usedAddrs = map cwamId ownOutAddrMetas
            changeAddrs = evalChange allAddresses (map cwamId ownInpAddrMetas) usedAddrs

        applyTxToUtxo (WithHash tx txId) taDistribution
        pure $ CAccModifier
            (deleteAndInsertMM [] ownOutAddrMetas camAddresses)
            (deleteAndInsertMM [] (zip usedAddrs hhs) camUsed)
            (deleteAndInsertMM [] (zip changeAddrs hhs) camChange)
            addedHistory
            camDeletedHistory

-- Process transactions on block rollback.
-- Like @trackingApplyTx@, but vise versa.
trackingRollbackTxs
    :: EncryptedSecretKey -- ^ Wallet's secret key
    -> [CWAddressMeta] -- ^ All adresses
    -> [(TxAux, TxUndo, HeaderHash)] -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingRollbackTxs (getEncInfo -> encInfo) allAddress txs =
    foldl' rollbackTx mempty txs
  where
    rollbackTx :: CAccModifier -> (TxAux, TxUndo, HeaderHash) -> CAccModifier
    rollbackTx CAccModifier{..} (TxAux {..}, NE.toList -> undoL, hh) = do
        let hhs = repeat hh
            UnsafeTx _ (toList -> outs) _ = taTx
            ownInputMetas = map snd . selectOwnAccounts encInfo (txOutAddress . toaOut) $ undoL
            ownOutputMetas = map snd . selectOwnAccounts encInfo txOutAddress $ outs
            ownInputAddrs = map cwamId ownInputMetas
            ownOutputAddrs = map cwamId ownOutputMetas

            deletedHistory =
                if (not $ null ownInputAddrs) || (not $ null ownOutputAddrs)
                then DL.snoc camDeletedHistory $ hash taTx
                else camDeletedHistory

        -- Rollback isn't needed, because we don't use @utxoGet@
        -- (undo contains all required information)
        let usedAddrs = map cwamId ownOutputMetas
            changeAddrs = evalChange allAddress ownInputAddrs ownOutputAddrs
        CAccModifier
            (deleteAndInsertMM ownOutputMetas [] camAddresses)
            (deleteAndInsertMM (zip usedAddrs hhs) [] camUsed)
            (deleteAndInsertMM (zip changeAddrs hhs) [] camChange)
            camAddedHistory
            deletedHistory

applyModifierToWallet
    :: WebWalletModeDB ctx m
    => CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
applyModifierToWallet wAddr newTip CAccModifier{..} = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ (WS.addWAddress . fst) (MM.insertions camAddresses)
    mapM_ (WS.addCustomAddress UsedAddr . fst) (MM.insertions camUsed)
    mapM_ (WS.addCustomAddress ChangeAddr . fst) (MM.insertions camChange)
    oldCachedHist <- fromMaybe [] <$> WS.getHistoryCache wAddr
    WS.updateHistoryCache wAddr $ DL.toList camAddedHistory <> oldCachedHist
    WS.setWalletSyncTip wAddr newTip

rollbackModifierFromWallet
    :: WebWalletModeDB ctx m
    => CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
rollbackModifierFromWallet wAddr newTip CAccModifier{..} = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ WS.removeWAddress (MM.deletions camAddresses)
    mapM_ (WS.removeCustomAddress UsedAddr) (MM.deletions camUsed)
    mapM_ (WS.removeCustomAddress ChangeAddr) (MM.deletions camChange)
    WS.getHistoryCache wAddr >>= \case
        Nothing -> pure ()
        Just oldCachedHist -> do
            WS.updateHistoryCache wAddr $
                removeFromHead (DL.toList camDeletedHistory) oldCachedHist
    WS.setWalletSyncTip wAddr newTip
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
    let wCId = addressToCId $ makePubKeyAddress pubKey
    (hdPass, wCId)

selectOwnAccounts
    :: (HDPassphrase, CId Wal)
    -> (a -> Address)
    -> [a]
    -> [(a, CWAddressMeta)]
selectOwnAccounts encInfo getAddr =
    mapMaybe (\a -> (a,) <$> decryptAccount encInfo (getAddr a))

deleteAndInsertMM :: (Eq a, Hashable a) => [a] -> [a] -> VoidModifier a -> VoidModifier a
deleteAndInsertMM dels ins mapModifier =
    -- Insert CWAddressMeta coressponding to outputs of tx.
    (\mm -> foldl' insertAcc mm ins) $
    -- Delete CWAddressMeta coressponding to inputs of tx.
    foldl' deleteAcc mapModifier dels
  where
    insertAcc :: (Hashable a, Eq a) => VoidModifier a -> a -> VoidModifier a
    insertAcc modifier acc = MM.insert acc () modifier

    deleteAcc :: (Hashable a, Eq a) => VoidModifier a -> a -> VoidModifier a
    deleteAcc modifier acc = MM.delete acc modifier

decryptAccount :: (HDPassphrase, CId Wal) -> Address -> Maybe CWAddressMeta
decryptAccount (hdPass, wCId) addr@(PubKeyAddress _ (Attributes (AddrPkAttrs (Just hdPayload)) _)) = do
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ CWAddressMeta wCId (derPath !! 0) (derPath !! 1) (addressToCId addr)
decryptAccount _ _ = Nothing

-- TODO [CSM-237] Move to somewhere (duplicate getWalletsAddrMetas from Methods.hs)
getWalletAddrMetasDB
    :: (WebWalletModeDB ctx m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m [CWAddressMeta]
getWalletAddrMetasDB lookupMode cWalId = do
    walletAccountIds <- filter ((== cWalId) . aiWId) <$> WS.getWAddressIds
    concatMapM (getAccountAddrsOrThrowDB lookupMode) walletAccountIds
  where
    getAccountAddrsOrThrowDB
        :: (WebWalletModeDB ctx m, MonadThrow m)
        => AddressLookupMode -> AccountId -> m [CWAddressMeta]
    getAccountAddrsOrThrowDB mode accId =
        WS.getAccountWAddresses mode accId >>= maybeThrow (noWallet accId)
    noWallet accId = DBMalformed $
        sformat ("No account with address "%build%" found") accId
