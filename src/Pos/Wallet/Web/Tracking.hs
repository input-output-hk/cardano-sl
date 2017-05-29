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
import           Pos.DB.Class               (MonadDB)
import qualified Pos.DB.DB                  as DB
import           Pos.DB.GState.BlockExtra   (foldlUpWhileM, resolveForwardLink)
import           Pos.Txp.Core               (Tx (..), TxAux (..), TxIn (..),
                                             TxOutAux (..), TxUndo, flattenTxPayload,
                                             getTxDistribution, toaOut, topsortTxs,
                                             txOutAddress)
import           Pos.Txp.MemState.Class     (MonadTxpMem, getLocalTxs)
import           Pos.Txp.Toil               (MonadUtxo (..), MonadUtxoRead (..), ToilT,
                                             evalToilTEmpty, runDBTxp)
import           Pos.Util.Chrono            (getNewestFirst)
import qualified Pos.Util.Modifier          as MM

import           Pos.Wallet.SscType         (WalletSscType)
import           Pos.Wallet.Web.ClientTypes (CAccountAddress (..), CAddress, WS,
                                             addressToCAddress, encToCAddress)
import           Pos.Wallet.Web.State       (WalletWebDB, WebWalletModeDB)
import qualified Pos.Wallet.Web.State       as WS

type CAccModifier = MM.MapModifier CAccountAddress ()

type BlockLockMode ssc m =
    ( WithLogger m
    , Ether.MonadReader' BlkSemaphore m
    , MonadDB m
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
                runDBTxp $ evalToilTEmpty $ trackingApplyTxs encSK ordered

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
    let (hdPass, wsAddr) = unzip $ map getEncInfo encSKs
    logDebug $ sformat ("Select accounts from Utxo: tip "%build%" for "%listJson) tip wsAddr
    addresses <- discoverHDAddresses hdPass
    let allAddreses = concatMap createAccounts $ zip wsAddr addresses
    mapM_ WS.addAccount allAddreses
    tip <$  logDebug (sformat ("After selection from Utxo addresses was added: "%listJson) allAddreses)
  where
    createAccounts :: (CAddress WS, [(Address, [Word32])]) -> [CAccountAddress]
    createAccounts (wsAddr, addresses) = do
        let (ads, paths) = unzip addresses
        mapMaybe createAccount $ zip3 (repeat wsAddr) ads paths

    createAccount :: (CAddress WS, Address, [Word32]) -> Maybe CAccountAddress
    createAccount (wsAddr, addr, derPath) = do
        guard $ length derPath == 2
        pure $ CAccountAddress wsAddr (derPath !! 0) (derPath !! 1) (addressToCAddress addr)

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
    , MonadDB m
    , DB.MonadBlockDB ssc m
    , WithLogger m)
    => EncryptedSecretKey
    -> m ()
syncWSetsWithGState encSK = do
    tipHeader <- DB.getTipHeader @(Block ssc)
    let wsAddr = encToCAddress encSK
    whenJustM (WS.getWSetSyncTip wsAddr) $ \wsTip ->
        if | wsTip == genesisHash && headerHash tipHeader == genesisHash ->
               logDebug $ sformat ("Walletset "%build%" at genesis state, synced") wsAddr
           | wsTip == genesisHash ->
               whenJustM (resolveForwardLink wsTip) $ \nx-> sync wsAddr nx tipHeader
           | otherwise -> sync wsAddr wsTip tipHeader
  where
    sync :: CAddress WS -> HeaderHash -> BlockHeader ssc -> m ()
    sync wsAddr wsTip tipHeader = DB.blkGetHeader wsTip >>= \case
        Nothing ->
            logWarning $
                sformat ("Couldn't get block header of walletset "%build
                         %" by last synced hh: "%build) wsAddr wsTip
        Just wsHeader -> do
            mapModifier <- compareHeaders wsAddr wsHeader tipHeader
            applyModifierToWSet wsAddr (headerHash tipHeader) mapModifier
            logDebug $ sformat ("Walletset "%build
                               %" has been synced with tip "%shortHashF%", added accounts: "%listJson
                               %", deleted accounts: "%listJson)
                       wsAddr wsTip
                       (map fst $ MM.insertions mapModifier)
                       (MM.deletions mapModifier)

    compareHeaders :: CAddress WS -> BlockHeader ssc -> BlockHeader ssc -> m CAccModifier
    compareHeaders wsAddr wsHeader tipHeader = do
        logDebug $
            sformat ("Walletset "%build%" header: "%build%", current tip header: "%build)
                    wsAddr wsHeader tipHeader
        if | diff tipHeader > diff wsHeader -> runDBTxp $ evalToilTEmpty $ do
            -- If walletset syncTip before the current tip,
            -- then it loads wallets starting with @wsHeader@.
            -- Sync tip can be before the current tip
            -- when we call @syncWalletSetWithTip@ at the first time
            -- or if the application was interrupted during rollback.
            -- We don't load blocks explicitly, because blockain can be long.
                maybe (pure mempty)
                      (\wsNextHeader -> foldlUpWhileM applyBlock wsNextHeader constTrue mappendR mempty)
                      =<< resolveForwardLink wsHeader
           | diff tipHeader < diff wsHeader -> do
            -- This rollback can occur
            -- if the application was interrupted during blocks application.
                blunds <- getNewestFirst <$>
                            DB.loadBlundsWhile (\b -> getBlockHeader b /= tipHeader) (headerHash wsHeader)
                pure $ foldl' (\r b -> r <> rollbackBlock b) mempty blunds
           | otherwise -> mempty <$ logInfo (sformat ("Walletset "%build%" is already synced") wsAddr)
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
    => CAddress WS
    -> HeaderHash
    -> CAccModifier
    -> m ()
applyModifierToWSet wsAddr newTip mapModifier = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ WS.removeAccount (MM.deletions mapModifier)
    mapM_ (WS.addAccount . fst) (MM.insertions mapModifier)
    WS.setWSetSyncTip wsAddr newTip

getEncInfo :: EncryptedSecretKey -> (HDPassphrase, CAddress WS)
getEncInfo encSK = do
    let pubKey = encToPublic encSK
    let hdPass = deriveHDPassphrase pubKey
    let wsCAddress = addressToCAddress $ makePubKeyAddress pubKey
    (hdPass, wsCAddress)

selectOwnAccounts
    :: (HDPassphrase, CAddress WS)
    -> (a -> Address)
    -> [a]
    -> [(a, CAccountAddress)]
selectOwnAccounts encInfo getAddr =
    mapMaybe (\a -> (a,) <$> decryptAccount encInfo (getAddr a))

deleteAndInsertMM :: [CAccountAddress] -> [CAccountAddress] -> CAccModifier -> CAccModifier
deleteAndInsertMM dels ins mapModifier =
    -- Insert CAccountAddress coressponding to outputs of tx.
    (\mm -> foldl' insertAcc mm ins) $
    -- Delete CAccountAddress coressponding to inputs of tx.
    foldl' deleteAcc mapModifier dels
  where
    insertAcc :: CAccModifier -> CAccountAddress -> CAccModifier
    insertAcc modifier acc = MM.insert acc () modifier

    deleteAcc :: CAccModifier -> CAccountAddress -> CAccModifier
    deleteAcc modifier acc = MM.delete acc modifier

decryptAccount :: (HDPassphrase, CAddress WS) -> Address -> Maybe CAccountAddress
decryptAccount (hdPass, wsCAddress) addr@(PubKeyAddress _ (Attributes (AddrPkAttrs (Just hdPayload)) _)) = do
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ CAccountAddress wsCAddress (derPath !! 0) (derPath !! 1) (addressToCAddress addr)
decryptAccount _ _ = Nothing
