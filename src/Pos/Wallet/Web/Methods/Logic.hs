{-# LANGUAGE TypeFamilies #-}

-- | Wallets, accounts and addresses management logic

module Pos.Wallet.Web.Methods.Logic
       ( getWallet
       , getWallets
       , getAccount
       , getAccounts
       , getWAddress

       , getWAddressBalance
       , getWalletAddrMetas
       , getWalletAddrs

       , createWalletSafe
       , newAccount
       , newAddress

       , deleteWallet
       , deleteAccount

       , updateWallet
       , renameWallet
       , updateAccount
       , changeWalletPassphrase

       , decodeCIdOrFail
       , decodeCAccountIdOrFail
       , decodeCCoinOrFail
       , getAccountAddrsOrThrow
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import           Data.List                  (findIndex, notElem)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (build, sformat, stext, (%))

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Core                   (Address, Coin, sumCoins, unsafeIntegerToCoin)
import           Pos.Crypto                 (PassPhrase, changeEncPassphrase,
                                             checkPassMatches, emptyPassphrase)
import           Pos.Util                   (maybeThrow)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Servant           (decodeCType, encodeCType)
import           Pos.Wallet.KeyStorage      (addSecretKey, deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode      (getBalance)
import           Pos.Wallet.Web.Account     (AddrGenSeed, genUniqueAccountAddress,
                                             genUniqueAccountId, getAddrIdx, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CAccount (..),
                                             CAccountId (..), CAccountInit (..),
                                             CAccountMeta (..), CAddress (..), CCoin, CId,
                                             CWAddressMeta (..), CWallet (..),
                                             CWalletMeta (..), Wal, addrMetaToAccount,
                                             cIdToAddress, coinFromCCoin, encToCId,
                                             mkCCoin)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.State       (AddressLookupMode (Existing),
                                             CustomAddressType (ChangeAddr, UsedAddr),
                                             addWAddress, createAccount, createWallet,
                                             getAccountIds, getAccountMeta,
                                             getAccountWAddresses, getWalletAddresses,
                                             getWalletMeta, getWalletPassLU,
                                             isCustomAddress, removeAccount,
                                             removeHistoryCache, removeTxMetas,
                                             removeWallet, setAccountMeta, setWalletMeta,
                                             setWalletPassLU)
import           Pos.Wallet.Web.Tracking    (CAccModifier (..), CachedCAccModifier,
                                             fixCachedAccModifierFor,
                                             fixingCachedAccModifier, sortedInsertions)
import           Pos.Wallet.Web.Util        (getWalletAccountIds)



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

updateWallet :: MonadWalletWebMode m => CId Wal -> CWalletMeta -> m CWallet
updateWallet wId wMeta = do
    setWalletMeta wId wMeta
    getWallet wId

renameWallet :: MonadWalletWebMode m => CId Wal -> Text -> m CWallet
renameWallet cid newName = do
    meta <- getWalletMeta cid >>= maybeThrow (RequestError "No such wallet")
    setWalletMeta cid meta{ cwName = newName }
    getWallet cid

updateAccount :: MonadWalletWebMode m => AccountId -> CAccountMeta -> m CAccount
updateAccount accId wMeta = do
    setAccountMeta accId wMeta
    fixingCachedAccModifier getAccount accId

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


