{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Wallet.Web.Tracking
       ( syncWSetsWithGStateLock
       , selectAccountsFromUtxoLock
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWSet
       , BlockLockMode
       , CAccModifier
       , MonadWalletTracking (..)
       ) where

import           Universum

import           Control.Lens               (to)
import           Control.Monad.Trans        (MonadTrans)
import           Data.List                  ((!!))
import qualified Data.List.NonEmpty         as NE
import qualified Ether
import           Formatting                 (build, sformat, (%))
import           Mockable                   (MonadMockable, SharedAtomicT)
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger, logDebug, logInfo, logWarning)

import           Pos.Block.Core             (Block, BlockHeader, getBlockHeader,
                                             mainBlockTxPayload)
import           Pos.Block.Logic            (withBlkSemaphore_)
import           Pos.Block.Types            (Blund, undoTx)
import           Pos.Constants              (genesisHash)
import           Pos.Context                (BlkSemaphore)
import           Pos.Core                   (AddrPkAttrs (..), Address (..),
                                             HasDifficulty (..), HeaderHash, headerHash,
                                             makePubKeyAddress)
import           Pos.Crypto                 (EncryptedSecretKey, HDPassphrase,
                                             WithHash (..), deriveHDPassphrase,
                                             encToPublic, hash, shortHashF,
                                             unpackHDAddressAttr)
import           Pos.Crypto.HDDiscovery     (discoverHDAddresses)
import           Pos.Data.Attributes        (Attributes (..))
import qualified Pos.DB.Block               as DB
import           Pos.DB.Class               (MonadRealDB)
import qualified Pos.DB.DB                  as DB
import           Pos.DB.GState.BlockExtra   (foldlUpWhileM, resolveForwardLink)
import           Pos.Txp.Core               (Tx (..), TxAux (..), TxIn (..),
                                             TxOutAux (..), TxUndo, flattenTxPayload,
                                             getTxDistribution, toaOut, topsortTxs,
                                             txOutAddress)
import           Pos.Txp.MemState.Class     (MonadTxpMem, getLocalTxs)
import           Pos.Txp.Toil               (MonadUtxo (..), MonadUtxoRead (..), ToilT,
                                             evalToilTEmpty, runDBToil)
import           Pos.Util.Chrono            (getNewestFirst)
import qualified Pos.Util.Modifier          as MM

import           Pos.Wallet.SscType         (WalletSscType)
import           Pos.Wallet.Web.ClientTypes (CId, CWAddressMeta (..), Wal, addressToCId,
                                             encToCId)
import           Pos.Wallet.Web.State       (WalletWebDB, WebWalletModeDB)
import qualified Pos.Wallet.Web.State       as WS

type CAccModifier = MM.MapModifier CWAddressMeta ()

type BlockLockMode ssc m =
    ( WithLogger m
    , Ether.MonadReader' BlkSemaphore m
    , MonadRealDB m
    , DB.MonadBlockDB ssc m
    , MonadMask m
    )

class Monad m => MonadWalletTracking m where
    syncWSetsAtStart :: [EncryptedSecretKey] -> m ()
    syncOnImport :: EncryptedSecretKey -> m ()
    txMempoolToModifier :: EncryptedSecretKey -> m CAccModifier


instance {-# OVERLAPPABLE #-}
    ( MonadWalletTracking m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        MonadWalletTracking (t m)
  where
    syncWSetsAtStart = lift . syncWSetsAtStart
    syncOnImport = lift . syncOnImport
    txMempoolToModifier = lift . txMempoolToModifier

instance (BlockLockMode WalletSscType m, MonadMockable m, MonadTxpMem ext m)
         => MonadWalletTracking (WalletWebDB m) where
    syncWSetsAtStart = syncWSetsWithGStateLock @WalletSscType
    syncOnImport = selectAccountsFromUtxoLock @WalletSscType . pure
    txMempoolToModifier encSK = do
        let wHash (i, TxAux {..}) = WithHash taTx i
        txs <- getLocalTxs
        case topsortTxs wHash txs of
            Nothing -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
            Just (map snd -> ordered) ->
                runDBToil $ evalToilTEmpty $ trackingApplyTxs encSK ordered

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

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

-- Select our accounts from Utxo and put to wallet-db.
-- Used for importing of a secret key.
selectAccountsFromUtxoLock
    :: forall ssc m . (WebWalletModeDB m, BlockLockMode ssc m)
    => [EncryptedSecretKey]
    -> m ()
selectAccountsFromUtxoLock encSKs = withBlkSemaphore_ $ \tip -> do
    let (hdPass, wAddr) = unzip $ map getEncInfo encSKs
    logDebug $ sformat ("Select accounts from Utxo: tip "%build%" for "%listJson) tip wAddr
    addresses <- discoverHDAddresses hdPass
    let allAddreses = concatMap createWAddresss $ zip wAddr addresses
    mapM_ WS.addWAddress allAddreses
    tip <$  logDebug (sformat ("After selection from Utxo addresses was added: "%listJson) allAddreses)
  where
    createWAddresss :: (CId Wal, [(Address, [Word32])]) -> [CWAddressMeta]
    createWAddresss (wAddr, addresses) = do
        let (ads, paths) = unzip addresses
        mapMaybe createWAddress $ zip3 (repeat wAddr) ads paths

    createWAddress :: (CId Wal, Address, [Word32]) -> Maybe CWAddressMeta
    createWAddress (wAddr, addr, derPath) = do
        guard $ length derPath == 2
        pure $ CWAddressMeta wAddr (derPath !! 0) (derPath !! 1) (addressToCId addr)

-- Iterate over blocks (using forward links) and actualize our accounts.
syncWSetsWithGStateLock
    :: forall ssc m . (WebWalletModeDB m, BlockLockMode ssc m)
    => [EncryptedSecretKey]
    -> m ()
syncWSetsWithGStateLock encSKs = withBlkSemaphore_ $ \tip ->
    tip <$ mapM_ (syncWSetsWithGState @ssc) encSKs

----------------------------------------------------------------------------
-- Unsafe operations. Core logic.
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take a lock.

syncWSetsWithGState
    :: forall ssc m .
    ( WebWalletModeDB m
    , MonadRealDB m
    , DB.MonadBlockDB ssc m
    , WithLogger m)
    => EncryptedSecretKey
    -> m ()
syncWSetsWithGState encSK = do
    tipHeader <- DB.getTipHeader @(Block ssc)
    let wAddr = encToCId encSK
    whenJustM (WS.getWalletSyncTip wAddr) $ \wTip ->
        if | wTip == genesisHash && headerHash tipHeader == genesisHash ->
               logDebug $ sformat ("Wallet "%build%" at genesis state, synced") wAddr
           | wTip == genesisHash ->
               whenJustM (resolveForwardLink wTip) $ \nx-> sync wAddr nx tipHeader
           | otherwise -> sync wAddr wTip tipHeader
  where
    sync :: CId Wal -> HeaderHash -> BlockHeader ssc -> m ()
    sync wAddr wTip tipHeader = DB.blkGetHeader wTip >>= \case
        Nothing ->
            logWarning $
                sformat ("Couldn't get block header of wallet "%build
                         %" by last synced hh: "%build) wAddr wTip
        Just wHeader -> do
            mapModifier <- compareHeaders wAddr wHeader tipHeader
            applyModifierToWSet wAddr (headerHash tipHeader) mapModifier
            logDebug $ sformat ("Wallet "%build
                               %" has been synced with tip "%shortHashF%", added addresses: "%listJson
                               %", deleted addresses: "%listJson)
                       wAddr wTip
                       (map fst $ MM.insertions mapModifier)
                       (MM.deletions mapModifier)

    compareHeaders :: CId Wal -> BlockHeader ssc -> BlockHeader ssc -> m CAccModifier
    compareHeaders wAddr wHeader tipHeader = do
        logDebug $
            sformat ("Wallet "%build%" header: "%build%", current tip header: "%build)
                    wAddr wHeader tipHeader
        if | diff tipHeader > diff wHeader -> runDBToil $ evalToilTEmpty $ do
            -- If walletset syncTip before the current tip,
            -- then it loads wallets starting with @wHeader@.
            -- Sync tip can be before the current tip
            -- when we call @syncWalletSetWithTip@ at the first time
            -- or if the application was interrupted during rollback.
            -- We don't load blocks explicitly, because blockain can be long.
                maybe (pure mempty)
                      (\wNextHeader -> foldlUpWhileM applyBlock wNextHeader constTrue mappendR mempty)
                      =<< resolveForwardLink wHeader
           | diff tipHeader < diff wHeader -> do
            -- This rollback can occur
            -- if the application was interrupted during blocks application.
                blunds <- getNewestFirst <$>
                            DB.loadBlundsWhile (\b -> getBlockHeader b /= tipHeader) (headerHash wHeader)
                pure $ foldl' (\r b -> r <> rollbackBlock b) mempty blunds
           | otherwise -> mempty <$ logInfo (sformat ("Wallet "%build%" is already synced") wAddr)
    constTrue = \_ _ -> True
    mappendR r mm = pure (r <> mm)
    diff = (^. difficultyL)
    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

    rollbackBlock :: Blund ssc -> CAccModifier
    rollbackBlock (b, u) = trackingRollbackTxs encSK $ zip (gbTxs b) (undoTx u)

    applyBlock :: (WithLogger m1, MonadUtxoRead m1)
               => Blund ssc -> ToilT () m1 CAccModifier
    applyBlock = trackingApplyTxs encSK . gbTxs . fst

-- Process transactions on block application,
-- decrypt our addresses, and add/delete them to/from wallet-db.
-- Addresses are used in TxIn's will be deleted,
-- in TxOut's will be added.
trackingApplyTxs
    :: forall m . MonadUtxo m
    => EncryptedSecretKey
    -> [TxAux]
    -> m CAccModifier
trackingApplyTxs (getEncInfo -> encInfo) txs =
    foldlM applyTx mempty txs
  where
    snd3 (_, x, _) = x
    applyTxOut txid (idx, out, dist) = utxoPut (TxIn txid idx) (TxOutAux out dist)
    applyTx :: CAccModifier -> TxAux -> m CAccModifier
    applyTx mapModifier TxAux {..} = do
        let tx@(UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = taTx
        let txid = hash tx
        resolvedInputs <- catMaybes <$> mapM (\tin -> fmap (tin, ) <$> utxoGet tin) inps
        let ownInputs = selectOwnAccounts encInfo (txOutAddress . toaOut . snd) resolvedInputs
        let ownOutputs = selectOwnAccounts encInfo (txOutAddress . snd3) $
                         zip3 [0..] outs (NE.toList $ getTxDistribution taDistribution)
        -- Delete and insert only own addresses to avoid large the underlying UtxoModifier.
        mapM_ (utxoDel . fst . fst) ownInputs -- del TxIn's (like in the applyTxToUtxo)
        mapM_ (applyTxOut txid . fst) ownOutputs -- add TxIn -> TxOutAux (like in the applyTxToUtxo)
        pure $ deleteAndInsertMM (map snd ownInputs) (map snd ownOutputs) mapModifier

-- Process transactions on block rollback.
-- Like @trackingApplyTx@, but vise versa.
trackingRollbackTxs
    :: EncryptedSecretKey
    -> [(TxAux, TxUndo)]
    -> CAccModifier
trackingRollbackTxs (getEncInfo -> encInfo) txs =
    foldl' rollbackTx mempty txs
  where
    rollbackTx :: CAccModifier -> (TxAux, TxUndo) -> CAccModifier
    rollbackTx mapModifier (TxAux {..}, NE.toList -> undoL) = do
        let UnsafeTx _ (toList -> outs) _ = taTx
        let ownInputs = map snd . selectOwnAccounts encInfo (txOutAddress . toaOut) $ undoL
        let ownOutputs = map snd . selectOwnAccounts encInfo txOutAddress $ outs
        -- Rollback isn't needed, because we don't use @utxoGet@
        -- (undo contains all required information)
        deleteAndInsertMM ownOutputs ownInputs mapModifier

applyModifierToWSet
    :: WebWalletModeDB m
    => CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
applyModifierToWSet wAddr newTip mapModifier = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ (WS.addWAddress . fst) (MM.insertions mapModifier)
    WS.setWalletSyncTip wAddr newTip

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

deleteAndInsertMM :: [CWAddressMeta] -> [CWAddressMeta] -> CAccModifier -> CAccModifier
deleteAndInsertMM dels ins mapModifier =
    -- Insert CWAddressMeta coressponding to outputs of tx.
    (\mm -> foldl' insertAcc mm ins) $
    -- Delete CWAddressMeta coressponding to inputs of tx.
    foldl' deleteAcc mapModifier dels
  where
    insertAcc :: CAccModifier -> CWAddressMeta -> CAccModifier
    insertAcc modifier acc = MM.insert acc () modifier

    deleteAcc :: CAccModifier -> CWAddressMeta -> CAccModifier
    deleteAcc modifier acc = MM.delete acc modifier

decryptAccount :: (HDPassphrase, CId Wal) -> Address -> Maybe CWAddressMeta
decryptAccount (hdPass, wCId) addr@(PubKeyAddress _ (Attributes (AddrPkAttrs (Just hdPayload)) _)) = do
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ CWAddressMeta wCId (derPath !! 0) (derPath !! 1) (addressToCId addr)
decryptAccount _ _ = Nothing
