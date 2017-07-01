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
       ( syncWalletsWithGState
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWallet
       , rollbackModifierFromWallet
       , BlockLockMode
       , CAccModifier (..)
       , MonadWalletTracking (..)

       , getWalletAddrMetasDB
       ) where

import           Universum

import           Control.Lens               (to)
import           Control.Monad.Trans        (MonadTrans)
import           Data.List                  ((!!))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
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
import           Pos.Context                (BlkSemaphore, genesisUtxoM, GenesisUtxo (..))
import           Pos.Core                   (AddrPkAttrs (..), Address (..),
                                             HasDifficulty (..), HeaderHash, headerHash,
                                             makePubKeyAddress)
import           Pos.Crypto                 (EncryptedSecretKey, HDPassphrase,
                                             WithHash (..), deriveHDPassphrase,
                                             encToPublic, hash, shortHashF,
                                             unpackHDAddressAttr)
import           Pos.Data.Attributes        (Attributes (..))
import qualified Pos.DB.Block               as DB
import           Pos.DB.Class               (MonadRealDB)
import qualified Pos.DB.DB                  as DB
import           Pos.DB.Error               (DBError (DBMalformed))
import           Pos.DB.GState.BlockExtra   (foldlUpWhileM, resolveForwardLink)
import           Pos.Txp.Core               (Tx (..), TxAux (..), TxIn (..),
                                             TxOutAux (..), TxUndo, flattenTxPayload,
                                             getTxDistribution, toaOut, topsortTxs,
                                             txOutAddress)
import           Pos.Txp.MemState.Class     (MonadTxpMem, getLocalTxs)
import           Pos.Txp.Toil               (MonadUtxo (..), MonadUtxoRead (..), ToilT, UtxoModifier,
                                             evalToilTEmpty, runDBTxp)
import           Pos.Util.Chrono            (getNewestFirst)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Modifier          (MapModifier)
import           Pos.Util.Util              (maybeThrow)

import           Pos.Wallet.SscType         (WalletSscType)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId,
                                             CWAddressMeta (..), Wal,
                                             addressToCId, aiWId, encToCId,
                                             isTxLocalAddress)
import           Pos.Wallet.Web.State       (AddressLookupMode (..),
                                             CustomAddressType (..), WalletWebDB,
                                             WebWalletModeDB)
import qualified Pos.Wallet.Web.State       as WS

-- VoidModifier describes a difference between two states.
-- It's (set of added k, set of deleted k) essentially.
type VoidModifier a = MapModifier a ()

data CAccModifier = CAccModifier {
      camAddresses :: !(VoidModifier CWAddressMeta)
    , camUsed      :: !(VoidModifier (CId Addr, HeaderHash))
    , camChange    :: !(VoidModifier (CId Addr, HeaderHash))
    , camUtxo      :: !UtxoModifier
    }

instance Monoid CAccModifier where
    mempty = CAccModifier mempty mempty mempty mempty
    (CAccModifier a b c d) `mappend` (CAccModifier a1 b1 c1 d1) =
        CAccModifier (a <> a1) (b <> b1) (c <> c1) (d <> d1)

type BlockLockMode ssc m =
    ( WithLogger m
    , Ether.MonadReader' BlkSemaphore m
    , MonadRealDB m
    , DB.MonadBlockDB ssc m
    , MonadMask m
    )

class Monad m => MonadWalletTracking m where
    syncWalletOnImport :: EncryptedSecretKey -> m ()
    txMempoolToModifier :: EncryptedSecretKey -> m CAccModifier

instance {-# OVERLAPPABLE #-}
    ( MonadWalletTracking m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        MonadWalletTracking (t m)
  where
    syncWalletOnImport = lift . syncWalletOnImport
    txMempoolToModifier = lift . txMempoolToModifier

instance ( BlockLockMode WalletSscType m
         , MonadMockable m
         , MonadTxpMem ext m
         , Ether.MonadReader' GenesisUtxo m)
         => MonadWalletTracking (WalletWebDB m) where
    syncWalletOnImport = syncWalletsWithGState @WalletSscType . one
    txMempoolToModifier encSK = do
        let wHash (i, TxAux {..}) = WithHash taTx i
        let wId = encToCId encSK
        txs <- getLocalTxs
        allAddresses <- getWalletAddrMetasDB Ever wId
        case topsortTxs wHash txs of
            Nothing -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
            Just (map snd -> ordered) ->
                runDBTxp $
                evalToilTEmpty $
                -- Hash doesn't matter
                trackingApplyTxs encSK allAddresses (zip ordered (repeat genesisHash))

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

-- Iterate over blocks (using forward links) and actualize our accounts.
syncWalletsWithGState
    :: forall ssc m . (
      WebWalletModeDB m,
      BlockLockMode ssc m,
      Ether.MonadReader' GenesisUtxo m)
    => [EncryptedSecretKey]
    -> m ()
syncWalletsWithGState encSKs = withBlkSemaphore_ $ \tip ->
    tip <$ mapM_ (syncWalletWithGStateUnsafe @ssc) encSKs

----------------------------------------------------------------------------
-- Unsafe operations. Core logic.
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take the block lock.

-- BE CAREFUL! This function iterates over blockchain, the blockcahin can be large.
syncWalletWithGStateUnsafe
    :: forall ssc m .
    ( WebWalletModeDB m
    , MonadRealDB m
    , DB.MonadBlockDB ssc m
    , Ether.MonadReader' GenesisUtxo m
    , WithLogger m)
    => EncryptedSecretKey
    -> m ()
syncWalletWithGStateUnsafe encSK = do
    tipHeader <- DB.getTipHeader @(Block ssc)
    let wAddr = encToCId encSK
    whenJustM (WS.getWalletSyncTip wAddr) $ \wTip ->
        if | wTip == genesisHash && headerHash tipHeader == genesisHash ->
               logDebug $ sformat ("Wallet "%build%" at genesis state, synced") wAddr
           | otherwise -> sync wAddr wTip tipHeader
  where
    sync :: CId Wal -> HeaderHash -> BlockHeader ssc -> m ()
    sync wAddr wTip tipHeader = do
        startFrom <-
            if wTip == genesisHash then
                resolveForwardLink wTip >>=
                maybe (error "Unexpected state: wTip doesn't have forward link") pure
            else pure wTip

        DB.blkGetHeader startFrom >>= \case
            Nothing ->
                logWarning $
                    sformat ("Couldn't get block header of wallet "%build
                            %" by last synced hh: "%build) wAddr wTip
            Just wHeader -> do
                mapModifier@CAccModifier{..} <- computeAccModifier wAddr wHeader tipHeader
                when (wTip == genesisHash) $ do
                    genesisUtxo <- genesisUtxoM
                    let encInfo = getEncInfo encSK
                    let ownGenesisUtxo =
                            M.fromList $
                            map fst $
                            selectOwnAccounts encInfo (txOutAddress . toaOut . snd) (M.toList genesisUtxo)
                    WS.getWalletUtxo >>= WS.setWalletUtxo . (ownGenesisUtxo <>)
                applyModifierToWallet wAddr (headerHash tipHeader) mapModifier
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

    computeAccModifier :: CId Wal -> BlockHeader ssc -> BlockHeader ssc -> m CAccModifier
    computeAccModifier wAddr wHeader tipHeader = do
        allAddresses <- getWalletAddrMetasDB Ever wAddr
        logDebug $
            sformat ("Wallet "%build%" header: "%build%", current tip header: "%build)
                    wAddr wHeader tipHeader
        if | diff tipHeader > diff wHeader -> runDBTxp $ evalToilTEmpty $ do
            -- If walletset syncTip before the current tip,
            -- then it loads wallets starting with @wHeader@.
            -- Sync tip can be before the current tip
            -- when we call @syncWalletSetWithTip@ at the first time
            -- or if the application was interrupted during rollback.
            -- We don't load blocks explicitly, because blockain can be long.
                maybe (pure mempty)
                      (\wNextHeader -> foldlUpWhileM (applyBlock allAddresses) wNextHeader constTrue mappendR mempty)
                      =<< resolveForwardLink wHeader
           | diff tipHeader < diff wHeader -> do
            -- This rollback can occur
            -- if the application was interrupted during blocks application.
                blunds <- getNewestFirst <$>
                            DB.loadBlundsWhile (\b -> getBlockHeader b /= tipHeader) (headerHash wHeader)
                pure $ foldl' (\r b -> r <> rollbackBlock allAddresses b) mempty blunds
           | otherwise -> mempty <$ logInfo (sformat ("Wallet "%build%" is already synced") wAddr)

    constTrue = \_ _ -> True
    mappendR r mm = pure (r <> mm)
    diff = (^. difficultyL)
    gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

    rollbackBlock :: [CWAddressMeta] -> Blund ssc -> CAccModifier
    rollbackBlock allAddresses (b, u) =
        trackingRollbackTxs encSK allAddresses (zip3 (gbTxs b) (undoTx u) (repeat $ headerHash b))

    applyBlock :: (WithLogger m1, MonadUtxoRead m1)
               => [CWAddressMeta] -> Blund ssc -> ToilT () m1 CAccModifier
    applyBlock allAddresses (b, _) =
        trackingApplyTxs encSK allAddresses $ zip (gbTxs b) (repeat $ headerHash b)

-- Process transactions on block application,
-- decrypt our addresses, and add/delete them to/from wallet-db.
-- Addresses are used in TxIn's will be deleted,
-- in TxOut's will be added.
trackingApplyTxs
    :: forall m . MonadUtxo m
    => EncryptedSecretKey    -- ^ Wallet's secret key
    -> [CWAddressMeta]       -- ^ All addresses in wallet
    -> [(TxAux, HeaderHash)] -- ^ Txs of blocks and corresponding header hash
    -> m CAccModifier
trackingApplyTxs (getEncInfo -> encInfo) allAddresses txs =
    foldlM applyTx mempty txs
  where
    snd3 (_, x, _) = x
    toTxInOut txid (idx, out, dist) = (TxIn txid idx, TxOutAux out dist)

    applyTx :: CAccModifier -> (TxAux, HeaderHash) -> m CAccModifier
    applyTx CAccModifier{..} (TxAux {..}, hh) = do
        let hhs = repeat hh
        let tx@(UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = taTx
        let !txid = hash tx
        resolvedInputs <- catMaybes <$> mapM (\tin -> fmap (tin, ) <$> utxoGet tin) inps
        let ownInputs = selectOwnAccounts encInfo (txOutAddress . toaOut . snd) resolvedInputs
        let ownOutputs = selectOwnAccounts encInfo (txOutAddress . snd3) $
                         zip3 [0..] outs (NE.toList $ getTxDistribution taDistribution)
        let ownTxIns = map (fst . fst) ownInputs
        let ownTxOuts = map (toTxInOut txid . fst) ownOutputs
        -- Delete and insert only own addresses to avoid large the underlying UtxoModifier.
        mapM_ utxoDel ownTxIns            -- del TxIn's (like in the applyTxToUtxo)
        mapM_ (uncurry utxoPut) ownTxOuts -- add TxIn -> TxOutAux (like in the applyTxToUtxo)
        let usedAddrs = map (cwamId . snd) ownOutputs
        let changeAddrs = evalChange allAddresses (map snd ownInputs) (map snd ownOutputs)
        pure $ CAccModifier
            (deleteAndInsertVM [] (map snd ownOutputs) camAddresses)
            (deleteAndInsertVM [] (zip usedAddrs  hhs) camUsed)
            (deleteAndInsertVM [] (zip changeAddrs hhs) camChange)
            (deleteAndInsertMM ownTxIns ownTxOuts camUtxo)

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
        let tx@(UnsafeTx (toList -> inps) (toList -> outs) _) = taTx
        let !txid = hash tx
        let ownInputs = selectOwnAccounts encInfo (txOutAddress . toaOut) $ undoL
        let ownOutputs = map snd . selectOwnAccounts encInfo txOutAddress $ outs
        -- Rollback isn't needed, because we don't use @utxoGet@
        -- (undo contains all required information)
        let l = fromIntegral (length outs) :: Word32
        let ownTxIns = zip inps (map fst ownInputs)
        let ownTxOuts = map (TxIn txid) ([0 .. l - 1] :: [Word32])
        let usedAddrs = map cwamId ownOutputs
        let changeAddrs = evalChange allAddress (map snd ownInputs) ownOutputs
        CAccModifier
            (deleteAndInsertVM ownOutputs [] camAddresses)
            (deleteAndInsertVM (zip usedAddrs hhs) [] camUsed)
            (deleteAndInsertVM (zip changeAddrs hhs) [] camChange)
            (deleteAndInsertMM ownTxOuts ownTxIns camUtxo)

applyModifierToWallet
    :: WebWalletModeDB m
    => CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
applyModifierToWallet wid newTip CAccModifier{..} = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ (WS.addWAddress . fst) (MM.insertions camAddresses)
    mapM_ (WS.addCustomAddress UsedAddr . fst) (MM.insertions camUsed)
    mapM_ (WS.addCustomAddress ChangeAddr . fst) (MM.insertions camChange)
    WS.getWalletUtxo >>= WS.setWalletUtxo . MM.modifyMap camUtxo
    WS.setWalletSyncTip wid newTip

rollbackModifierFromWallet
    :: WebWalletModeDB m
    => CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
rollbackModifierFromWallet wid newTip CAccModifier{..} = do
    -- TODO maybe do it as one acid-state transaction.
    mapM_ WS.removeWAddress (MM.deletions camAddresses)
    mapM_ (WS.removeCustomAddress UsedAddr) (MM.deletions camUsed)
    mapM_ (WS.removeCustomAddress ChangeAddr) (MM.deletions camChange)
    WS.getWalletUtxo >>= WS.setWalletUtxo . MM.modifyMap camUtxo
    WS.setWalletSyncTip wid newTip

evalChange
    :: [CWAddressMeta] -- ^ All adresses
    -> [CWAddressMeta] -- ^ Own input addresses of tx
    -> [CWAddressMeta] -- ^ Own outputs addresses of tx
    -> [CId Addr]
evalChange allAddresses (map cwamId -> inputs) (map cwamId -> outputs)
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

decryptAccount :: (HDPassphrase, CId Wal) -> Address -> Maybe CWAddressMeta
decryptAccount (hdPass, wCId) addr@(PubKeyAddress _ (Attributes (AddrPkAttrs (Just hdPayload)) _)) = do
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ CWAddressMeta wCId (derPath !! 0) (derPath !! 1) (addressToCId addr)
decryptAccount _ _ = Nothing

-- TODO [CSM-237] Move to somewhere (duplicate getWalletsAddrMetas from Methods.hs)
getWalletAddrMetasDB
    :: (WebWalletModeDB m, MonadThrow m)
    => AddressLookupMode -> CId Wal -> m [CWAddressMeta]
getWalletAddrMetasDB lookupMode cWalId = do
    walletAccountIds <- filter ((== cWalId) . aiWId) <$> WS.getWAddressIds
    concatMapM (getAccountAddrsOrThrowDB lookupMode) walletAccountIds
  where
    getAccountAddrsOrThrowDB
        :: (WebWalletModeDB m, MonadThrow m)
        => AddressLookupMode -> AccountId -> m [CWAddressMeta]
    getAccountAddrsOrThrowDB mode accId =
        WS.getAccountWAddresses mode accId >>= maybeThrow (noWallet accId)
    noWallet accId = DBMalformed $
        sformat ("No account with address "%build%" found") accId

deleteAndInsertVM :: (Eq a, Hashable a) => [a] -> [a] -> VoidModifier a -> VoidModifier a
deleteAndInsertVM dels ins mapModifier = deleteAndInsertMM dels (zip ins $ repeat ()) mapModifier

deleteAndInsertMM :: (Eq k, Hashable k) => [k] -> [(k, v)] -> MM.MapModifier k v -> MM.MapModifier k v
deleteAndInsertMM dels ins mapModifier =
    -- Insert CWAddressMeta coressponding to outputs of tx (2)
    (\mm -> foldl' insertAcc mm ins) $
    -- Delete CWAddressMeta coressponding to inputs of tx (1)
    foldl' deleteAcc mapModifier dels
  where
    insertAcc :: (Hashable k, Eq k) => MapModifier k v -> (k, v) -> MapModifier k v
    insertAcc modifier (k, v) = MM.insert k v modifier

    deleteAcc :: (Hashable k, Eq k) => MapModifier k v -> k -> MapModifier k v
    deleteAcc = flip MM.delete

-- Select our accounts from Utxo and put to wallet-db.
-- Used for importing of a secret key.
-- This function DOESN'T synchronize Change and Used addresses.
-- Usages of this function will be removed soon,
-- but it won't be removed, because frontenders
-- can change logic of Used and Change andresses
-- and this function will be useful again
-- syncAddressesWithUtxo
--     :: forall ssc m . (WebWalletModeDB m, BlockLockMode ssc m)
--     => [EncryptedSecretKey]
--     -> m [CWAddressMeta]
-- syncAddressesWithUtxo encSKs =
--     withBlkSemaphore $ \tip -> (, tip) <$> syncAddressesWithUtxoUnsafe @ssc tip encSKs

-- BE CAREFUL! This function iterates over Utxo,
-- the Utxo can be large (but not such large as the blockchain)
-- syncAddressesWithUtxoUnsafe
--     :: forall ssc m .
--     ( WebWalletModeDB m
--     , MonadRealDB m
--     , DB.MonadBlockDB ssc m
--     , WithLogger m)
--     => HeaderHash
--     -> [EncryptedSecretKey]
--     -> m [CWAddressMeta]
-- syncAddressesWithUtxoUnsafe tip encSKs = do
--     let (hdPass, wAddr) = unzip $ map getEncInfo encSKs
--     logDebug $ sformat ("Sync addresses with Utxo: tip "%build%" for "%listJson) tip wAddr
--     addresses <- discoverHDAddresses hdPass
--     let allAddresses = concatMap createWAddresses $ zip wAddr addresses
--     mapM_ addMetaInfo allAddresses
--     logDebug (sformat ("After syncing with Utxo addresses was added: "%listJson) allAddresses)
--     pure allAddresses
--   where
--     createWAddresses :: (CId Wal, [(Address, [Word32])]) -> [CWAddressMeta]
--     createWAddresses (wAddr, addresses) = do
--         let (ads, paths) = unzip addresses
--         mapMaybe createWAddress $ zip3 (repeat wAddr) ads paths

--     createWAddress :: (CId Wal, Address, [Word32]) -> Maybe CWAddressMeta
--     createWAddress (wAddr, addr, derPath) = do
--         guard $ length derPath == 2
--         pure $ CWAddressMeta wAddr (derPath !! 0) (derPath !! 1) (addressToCId addr)

--     addMetaInfo :: CWAddressMeta -> m ()
--     addMetaInfo cwMeta = do
--         let accId = addrMetaToAccount cwMeta
--             accMeta = CAccountMeta
--                       { caName = sformat ("Account #"%int) $ aiIndex accId
--                       }
--         WS.createAccount accId accMeta
--         WS.addWAddress cwMeta
