{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Wallet.Web.Tracking
       ( syncWalletSetsWithTipLock
       , syncWalletSetWithTip
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWSet
       , BlockLockMode
       , CAccModifier
       ) where

import           Control.Monad.Catch        (bracketOnError)
import           Data.List                  ((!!))
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, (%))
import           Serokell.Util              (listJson)
import           System.Wlog                (WithLogger, logDebug, logInfo, logWarning)
import           Universum

import           Pos.Block.Pure             (genesisHash)
import           Pos.Block.Types            (Blund, undoTx)
import           Pos.Context                (WithNodeContext, putBlkSemaphore,
                                             takeBlkSemaphore)
import           Pos.Core                   (HasDifficulty (..))
import           Pos.Core.Address           (AddrPkAttrs (..), Address (..),
                                             makePubKeyAddress)
import           Pos.Crypto                 (EncryptedSecretKey, HDPassphrase,
                                             deriveHDPassphrase, encToPublic, hash,
                                             shortHashF, unpackHDAddressAttr)
import           Pos.Data.Attributes        (Attributes (..))
import qualified Pos.DB.Block               as DB
import           Pos.DB.Class               (MonadDB)
import qualified Pos.DB.DB                  as DB
import           Pos.DB.GState.BlockExtra   (foldlUpWhileM, resolveForwardLink)
import           Pos.Ssc.Class              (SscHelpersClass)
import           Pos.Txp.Core               (Tx (..), TxAux, TxIn (..), TxOutAux (..),
                                             TxUndo, getTxDistribution, toaOut,
                                             txOutAddress)
import           Pos.Txp.Toil               (MonadUtxo (..), MonadUtxoRead (..), ToilT,
                                             evalToilTEmpty, runDBTxp)
import           Pos.Types                  (BlockHeader, HeaderHash, blockTxas,
                                             getBlockHeader, headerHash)
import           Pos.Util.Chrono            (getNewestFirst)
import qualified Pos.Util.Modifier          as MM

import           Pos.Wallet.Web.ClientTypes (CAccountAddress (..), CAddress, WS,
                                             addressToCAddress, encToCAddress)
import           Pos.Wallet.Web.State       (WebWalletModeDB)
import qualified Pos.Wallet.Web.State       as WS

type CAccModifier = MM.MapModifier CAccountAddress ()

type BlockLockMode ssc m =
    ( SscHelpersClass ssc
    , WithLogger m
    , WithNodeContext ssc m
    , MonadDB m
    , MonadMask m)

syncWalletSetsWithTipLock
    :: forall ssc m . (WebWalletModeDB m, BlockLockMode ssc m)
    => [EncryptedSecretKey]
    -> m ()
syncWalletSetsWithTipLock encSKs = withBlkSemaphore $ \tip ->
    tip <$ mapM_ (syncWalletSetWithTip @ssc) encSKs

-- | Run action acquiring lock on block application.
-- Argument of action is an old tip, result is put as a new tip.
-- Wallet version (without extra constraints)
withBlkSemaphore
    :: forall ssc m . (WebWalletModeDB m, BlockLockMode ssc m)
    => (HeaderHash -> m HeaderHash) -> m ()
withBlkSemaphore action =
    bracketOnError takeBlkSemaphore putBlkSemaphore doAction
  where
    doAction tip = action tip >>= putBlkSemaphore

----------------------------------------------------------------------------
-- Unsafe operations
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take a lock.

syncWalletSetWithTip
    :: forall ssc m .
    ( WebWalletModeDB m
    , MonadDB m
    , WithLogger m
    , SscHelpersClass ssc)
    => EncryptedSecretKey
    -> m ()
syncWalletSetWithTip encSK = do
    tipHeader <- DB.getTipBlockHeader @ssc
    let wsAddr = encToCAddress encSK
    whenJustM (WS.getWSetSyncTip wsAddr) $ \wsTip ->
        if | wsTip == genesisHash && headerHash tipHeader == genesisHash ->
               logDebug $ sformat ("Walletset "%build%" at genesis state, synced") wsAddr
           | wsTip == genesisHash ->
               whenJustM (resolveForwardLink wsTip) $ \nx-> sync wsAddr nx tipHeader
           | otherwise -> sync wsAddr wsTip tipHeader
  where
    sync :: CAddress WS -> HeaderHash -> BlockHeader ssc -> m ()
    sync wsAddr wsTip tipHeader = DB.getBlockHeader wsTip >>= \case
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
            -- If wallet set sync tip before the current tip,
            -- then load wallets starting with @wsHeader@.
            -- Sync tip can be before the current tip
            -- when we call @syncWalletSetWithTip@ at the first time
            -- or if the application was interrupted during rollback.
            -- We don't load blocks explicitly, because blockain can be long (at the first call).
                maybe (pure mempty)
                      (\wsNextHeader -> foldlUpWhileM applyBlock wsNextHeader constTrue mappendR mempty)
                      =<< resolveForwardLink wsHeader
           | diff tipHeader < diff wsHeader -> do
            -- This rollback can occur
            -- if the application was interrupted during blocks application.
            -- We can load block explicitly because this code can be called only
            -- when blocks application was interrupted (at the previous launch).
                blunds <- getNewestFirst <$>
                            DB.loadBlundsWhile (\b -> getBlockHeader b /= tipHeader) (headerHash wsHeader)
                pure $ foldl' (\r b -> r <> rollbackBlock b) mempty blunds
           | otherwise -> mempty <$ logInfo (sformat ("Walletset "%build%" is already synced") wsAddr)
    constTrue = \_ _ -> True
    mappendR r mm = pure (r <> mm)
    diff = (^. difficultyL)
    gbTxs = either (const []) (^. blockTxas)

    rollbackBlock :: Blund ssc -> CAccModifier
    rollbackBlock (b, u) = trackingRollbackTxs encSK $ zip (gbTxs b) (undoTx u)

    applyBlock :: (WithLogger m1, MonadUtxoRead m1)
               => Blund ssc -> ToilT () m1 CAccModifier
    applyBlock = trackingApplyTxs encSK . gbTxs . fst

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
    applyTx mapModifier (tx@(UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _), _, distr) = do
        let txid = hash tx
        resolvedInputs <- catMaybes <$> mapM (\tin -> fmap (tin, ) <$> utxoGet tin) inps
        let ownInputs = selectOwnAccounts encInfo (txOutAddress . toaOut . snd) resolvedInputs
        let ownOutputs = selectOwnAccounts encInfo (txOutAddress . snd3) $
                         zip3 [0..] outs (NE.toList $ getTxDistribution distr)
        -- Delete and insert only own addresses to avoid large the underlying UtxoModifier.
        mapM_ (utxoDel . fst . fst) ownInputs -- del TxIn's (like in the applyTxToUtxo)
        mapM_ (applyTxOut txid . fst) ownOutputs -- add TxIn -> TxOutAux (like in the applyTxToUtxo)
        pure $ deleteAndInsertMM (map snd ownInputs) (map snd ownOutputs) mapModifier

trackingRollbackTxs
    :: EncryptedSecretKey
    -> [(TxAux, TxUndo)]
    -> CAccModifier
trackingRollbackTxs (getEncInfo -> encInfo) txs =
    foldl' rollbackTx mempty txs
  where
    rollbackTx :: CAccModifier -> (TxAux, TxUndo) -> CAccModifier
    rollbackTx mapModifier ((UnsafeTx _ (NE.toList -> outs) _, _, _), NE.toList -> undoL) = do
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
