{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Methods.Logic
       (
       ) where

import           Universum

import           Control.Lens               (each, has, ix, traversed)
import           Control.Monad.Catch        (SomeException, try)
import qualified Control.Monad.Catch        as E
import qualified Data.Aeson                 as A
import           Data.ByteString.Base58     (bitcoinAlphabet, decodeBase58)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Default               (Default (def))
import qualified Data.DList                 as DL
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (findIndex, notElem)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as S
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (bprint, build, sformat, shown, stext, (%))
import qualified Formatting                 as F
import           Pos.ReportServer.Report    (ReportType (RInfo))
import qualified Serokell.Util.Base64       as B64
import           Serokell.Util.Text         (listJson)
import           Servant.Multipart          (fdFilePath)
import           System.IO.Error            (isDoesNotExistError)
import           System.Wlog                (logDebug, logError, logInfo, logWarning)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Binary.Class           (biSize)
import           Pos.Block.Logic.Util       (withBlkSemaphore_)
import           Pos.Client.Txp.Balances    (getOwnUtxos)
import           Pos.Client.Txp.History     (TxHistoryEntry (..))
import           Pos.Client.Txp.Util        (TxError (..), createMTx, overrideTxDistrBoot,
                                             overrideTxOutDistrBoot)
import           Pos.Communication          (SendActions (..), submitMTx,
                                             submitRedemptionTx)
import           Pos.Constants              (isDevelopment)
import           Pos.Core                   (Address (..), Coin, TxFeePolicy (..),
                                             TxSizeLinear (..), addressF, bvdTxFeePolicy,
                                             calculateTxSizeLinear, decodeTextAddress,
                                             getCurrentTimestamp, getTimestamp,
                                             integerToCoin, makeRedeemAddress, mkCoin,
                                             sumCoins, unsafeAddCoin, unsafeIntegerToCoin,
                                             unsafeSubCoin, _RedeemAddress)
import           Pos.Crypto                 (EncryptedSecretKey, PassPhrase, SafeSigner,
                                             aesDecrypt, changeEncPassphrase,
                                             checkPassMatches, deriveAesKeyBS,
                                             emptyPassphrase, fakeSigner, hash, keyGen,
                                             redeemDeterministicKeyGen, redeemToPublic,
                                             withSafeSigner, withSafeSigner)
import           Pos.DB.Class               (gsAdoptedBVData)
import           Pos.Genesis                (genesisDevHdwSecretKeys)
import           Pos.Reporting.MemState     (HasReportServers (..),
                                             HasReportingContext (..))
import           Pos.Reporting.Methods      (sendReport, sendReportNodeNologs)
import           Pos.Txp                    (TxFee (..))
import           Pos.Txp.Core               (TxAux (..), TxOut (..), TxOutAux (..),
                                             TxOutDistribution)
import           Pos.Util                   (eitherToThrow, maybeThrow)
import           Pos.Util.BackupPhrase      (toSeed)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Servant           (decodeCType, encodeCType)
import           Pos.Util.UserSecret        (UserSecretDecodingError (..), readUserSecret,
                                             usWalletSet)
import           Pos.Wallet.KeyStorage      (addSecretKey, deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode      (applyLastUpdate, connectedPeers, getBalance,
                                             getLocalHistory, localChainDifficulty,
                                             networkChainDifficulty)
import           Pos.Wallet.Web.Account     (AddrGenSeed, GenSeed (..),
                                             MonadKeySearch (..), genSaveRootKey,
                                             genUniqueAccountAddress, genUniqueAccountId,
                                             getAddrIdx, getSKById)
import           Pos.Wallet.Web.Backup      (AccountMetaBackup (..), StateBackup (..),
                                             WalletBackup (..), WalletMetaBackup (..),
                                             getStateBackup)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CAccount (..),
                                             CAccountId (..), CAccountInit (..),
                                             CAccountMeta (..), CAddress (..), CCoin,
                                             CElectronCrashReport (..), CId, CInitialized,
                                             CPaperVendWalletRedeem (..), CProfile,
                                             CProfile (..), CTx (..), CTxId, CTxMeta (..),
                                             CTxs (..), CUpdateInfo (..),
                                             CWAddressMeta (..), CWallet (..),
                                             CWalletInit (..), CWalletMeta (..),
                                             CWalletRedeem (..), SyncProgress (..), Wal,
                                             addrMetaToAccount, cIdToAddress,
                                             coinFromCCoin, encToCId, mkCCoin, mkCTxs,
                                             txIdToCTxId)
import           Pos.Wallet.Web.Error       (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.Secret      (WalletUserSecret (..),
                                             mkGenesisWalletUserSecret, wusAccounts,
                                             wusWalletName)
import           Pos.Wallet.Web.State       (AddressLookupMode (Ever, Existing),
                                             CustomAddressType (ChangeAddr, UsedAddr),
                                             addOnlyNewTxMeta, addWAddress, createAccount,
                                             createWallet, getAccountIds, getAccountMeta,
                                             getAccountWAddresses, getHistoryCache,
                                             getNextUpdate, getProfile, getTxMeta,
                                             getWalletAddresses, getWalletMeta,
                                             getWalletPassLU, isCustomAddress,
                                             removeAccount, removeHistoryCache,
                                             removeNextUpdate, removeTxMetas,
                                             removeWallet, setAccountMeta, setProfile,
                                             setWalletMeta, setWalletPassLU,
                                             setWalletSyncTip, setWalletTxMeta, testReset,
                                             updateHistoryCache)
import           Pos.Wallet.Web.Tracking    (CAccModifier (..), CachedCAccModifier,
                                             fixCachedAccModifierFor,
                                             fixingCachedAccModifier, sortedInsertions,
                                             syncWalletOnImport)
import           Pos.Wallet.Web.Util        (getWalletAccountIds, rewrapTxError)



----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getWAddressBalance :: MonadWalletWebMode m => CWAddressMeta -> m Coin
getWAddressBalance addr =
    getBalance <=< decodeCIdOrFail $ cwamId addr

getWAddress
    :: MonadWalletWebMode m
    => CachedCAccModifier -> CWAddressMeta -> m CAddress
getWAddress cachedAccModifier cAddr = do
    let aId = cwamId cAddr
    balance <- getWAddressBalance cAddr

    let getFlag customType accessMod = do
            checkDB <- isCustomAddress customType (cwamId cAddr)
            let checkMempool = elem aId . map (fst . fst) . toList $
                               MM.insertions $ accessMod cachedAccModifier
            return (checkDB || checkMempool)
    isUsed   <- getFlag UsedAddr camUsed
    isChange <- getFlag ChangeAddr camChange
    return $ CAddress aId (mkCCoin balance) isUsed isChange

getAccountAddrsOrThrow
    :: MonadWalletWebMode m
    => AddressLookupMode -> AccountId -> m [CWAddressMeta]
getAccountAddrsOrThrow mode accId =
    getAccountWAddresses mode accId >>= maybeThrow noWallet
  where
    noWallet =
        RequestError $
        sformat ("No account with id "%build%" found") accId

getAccount :: MonadWalletWebMode m => CachedCAccModifier -> AccountId -> m CAccount
getAccount accMod accId = do
    dbAddrs    <- getAccountAddrsOrThrow Existing accId
    let modifier   = camAddresses accMod
    let allAddrIds = gatherAddresses modifier dbAddrs
    allAddrs <- mapM (getWAddress accMod) allAddrIds
    balance  <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
                mapM getWAddressBalance allAddrIds
    meta <- getAccountMeta accId >>= maybeThrow noWallet
    pure $ CAccount (encodeCType accId) meta allAddrs balance
  where
    noWallet =
        RequestError $ sformat ("No account with id "%build%" found") accId
    gatherAddresses modifier dbAddrs = do
        let memAddrs = sortedInsertions modifier
            relatedMemAddrs = filter ((== accId) . addrMetaToAccount) memAddrs
            -- @|relatedMemAddrs|@ is O(1) while @dbAddrs@ is large
            unknownMemAddrs = filter (`notElem` dbAddrs) relatedMemAddrs
        dbAddrs <> unknownMemAddrs

getWallet :: MonadWalletWebMode m => CId Wal -> m CWallet
getWallet cAddr = do
    meta       <- getWalletMeta cAddr >>= maybeThrow noWSet
    wallets    <- getAccounts (Just cAddr)
    let walletsNum = length wallets
    balance    <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
                     mapM (decodeCCoinOrFail . caAmount) wallets
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKById cAddr
    passLU     <- getWalletPassLU cAddr >>= maybeThrow noWSet
    pure $ CWallet cAddr meta walletsNum balance hasPass passLU
  where
    noWSet = RequestError $
        sformat ("No wallet with address "%build%" found") cAddr

-- TODO [CSM-407]: Write smth like `decodeCTypeOrFail` instead of next functions
decodeCIdOrFail :: MonadThrow m => CId w -> m Address
decodeCIdOrFail = either wrongAddress pure . cIdToAddress
  where wrongAddress err = throwM . DecodeError $
            sformat ("Error while decoding CId: "%stext) err

decodeCAccountIdOrFail :: MonadThrow m => CAccountId -> m AccountId
decodeCAccountIdOrFail = either wrongAddress pure . decodeCType
  where wrongAddress err = throwM . DecodeError $
            sformat ("Error while decoding CAccountId: "%stext) err

decodeCCoinOrFail :: MonadThrow m => CCoin -> m Coin
decodeCCoinOrFail c =
    coinFromCCoin c `whenNothing` throwM (DecodeError "Wrong coin format")

getWalletAddrMetas
    :: MonadWalletWebMode m
    => AddressLookupMode -> CId Wal -> m [CWAddressMeta]
getWalletAddrMetas lookupMode cWalId =
    concatMapM (getAccountAddrsOrThrow lookupMode) =<<
    getWalletAccountIds cWalId

getWalletAddrs
    :: MonadWalletWebMode m
    => AddressLookupMode -> CId Wal -> m [CId Addr]
getWalletAddrs = (cwamId <<$>>) ... getWalletAddrMetas

getAccounts
    :: MonadWalletWebMode m
    => Maybe (CId Wal) -> m [CAccount]
getAccounts mCAddr = do
    whenJust mCAddr $ \cAddr -> getWalletMeta cAddr `whenNothingM_` noWSet cAddr
    accIds <- maybe getAccountIds getWalletAccountIds mCAddr
    let groupedAccIds = fmap reverse $ HM.fromListWith mappend $
                        accIds <&> \acc -> (aiWId acc, [acc])
    concatForM (HM.toList groupedAccIds) $ \(wid, walAccIds) ->
         fixCachedAccModifierFor wid $ forM walAccIds . getAccount
  where
    noWSet cAddr = throwM . RequestError $
        sformat ("No account with id "%build%" found") cAddr

getWallets :: MonadWalletWebMode m => m [CWallet]
getWallets = getWalletAddresses >>= mapM getWallet

                   -- When balance >= reqCoins,
                   -- then lets try to find input with exactly @reqCoins@ coins,
                   -- in order to use one address instead of two.
                   maybe (Right (balance `unsafeSubCoin` reqCoins, (ad, balance) :| []))
                         (\fa -> Right (mkCoin 0, fa :| []))
                         (find ((reqCoins ==) . snd) addresses)

----------------------------------------------------------------------------
-- Creators
----------------------------------------------------------------------------

newAddress
    :: MonadWalletWebMode m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newAddress addGenSeed passphrase accId =
    fixCachedAccModifierFor accId $ \accMod -> do
        -- check whether account exists
        _ <- getAccount accMod accId

        cAccAddr <- genUniqueAccountAddress addGenSeed passphrase accId
        addWAddress cAccAddr
        getWAddress accMod cAccAddr

newAccount
    :: MonadWalletWebMode m
    => AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccount addGenSeed passphrase CAccountInit {..} =
    fixCachedAccModifierFor caInitWId $ \accMod -> do
        -- check wallet exists
        _ <- getWallet caInitWId

        cAddr <- genUniqueAccountId addGenSeed caInitWId
        createAccount cAddr caInitMeta
        () <$ newAddress addGenSeed passphrase cAddr
        getAccount accMod cAddr

createWalletSafe
    :: MonadWalletWebMode m
    => CId Wal -> CWalletMeta -> m CWallet
createWalletSafe cid wsMeta = do
    wSetExists <- isJust <$> getWalletMeta cid
    when wSetExists $
        throwM $ RequestError "Wallet with that mnemonics already exists"
    curTime <- liftIO getPOSIXTime
    createWallet cid wsMeta curTime
    getWallet cid

----------------------------------------------------------------------------
-- Deleters
----------------------------------------------------------------------------

deleteWallet :: MonadWalletWebMode m => CId Wal -> m ()
deleteWallet wid = do
    accounts <- getAccounts (Just wid)
    mapM_ (deleteAccount <=< decodeCAccountIdOrFail . caId) accounts
    removeWallet wid
    removeTxMetas wid
    removeHistoryCache wid
    deleteSecretKey . fromIntegral =<< getAddrIdx wid

deleteAccount :: MonadWalletWebMode m => AccountId -> m ()
deleteAccount = removeAccount

----------------------------------------------------------------------------
-- Modifiers
----------------------------------------------------------------------------

updateWallet :: WalletWebMode m => CId Wal -> CWalletMeta -> m CWallet
updateWallet wId wMeta = do
    setWalletMeta wId wMeta
    getWallet wId

updateAccount :: WalletWebMode m => AccountId -> CAccountMeta -> m CAccount
updateAccount accId wMeta = do
    setAccountMeta accId wMeta
    fixingCachedAccModifier getAccount accId

-- TODO [CSM-407]: Remove or rename
renameWSet :: MonadWalletWebMode m => CId Wal -> Text -> m CWallet
renameWSet cid newName = do
    meta <- getWalletMeta cid >>= maybeThrow (RequestError "No such wallet")
    setWalletMeta cid meta{ cwName = newName }
    getWallet cid

changeWalletPassphrase
    :: MonadWalletWebMode m
    => CId Wal -> PassPhrase -> PassPhrase -> m ()
changeWalletPassphrase wid oldPass newPass = do
    oldSK <- getSKById wid

    unless (isJust $ checkPassMatches newPass oldSK) $ do
        newSK <- maybeThrow badPass $ changeEncPassphrase oldPass newPass oldSK
        deleteSK oldPass
        addSecretKey newSK
        setWalletPassLU wid =<< liftIO getPOSIXTime
  where
    badPass = RequestError "Invalid old passphrase given"
    deleteSK passphrase = do
        let nice k = encToCId k == wid && isJust (checkPassMatches passphrase k)
        midx <- findIndex nice <$> getSecretKeys
        idx  <- RequestError "No key with such address and pass found"
                `maybeThrow` midx
        deleteSecretKey (fromIntegral idx)


