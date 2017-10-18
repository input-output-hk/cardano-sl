{-# LANGUAGE TypeFamilies #-}

-- | Wallets, accounts and addresses management logic

module Pos.Wallet.Web.Methods.Logic
       ( getWallet
       , getWallets
       , getAccount
       , getAccounts

       , createWalletSafe
       , newAccount
       , newAccountIncludeUnready
       , newAddress
       , markWalletReady

       , deleteWallet
       , deleteAccount

       , updateWallet
       , updateAccount
       , changeWalletPassphrase
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import           Data.List                  (findIndex, notElem)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (build, sformat, (%))

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Core                   (Coin, sumCoins, unsafeIntegerToCoin)
import           Pos.Crypto                 (PassPhrase, changeEncPassphrase,
                                             checkPassMatches, emptyPassphrase)
import           Pos.Util                   (maybeThrow)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.KeyStorage      (addSecretKey, deleteSecretKey,
                                             getSecretKeysPlain)
import           Pos.Wallet.WalletMode      (getBalance)
import           Pos.Wallet.Web.Account     (AddrGenSeed, genUniqueAccountId,
                                             genUniqueAddress, getAddrIdx, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccount (..),
                                             CAccountInit (..), CAccountMeta (..),
                                             CAddress (..), CId, CWAddressMeta (..),
                                             CWallet (..), CWalletMeta (..), Wal,
                                             addrMetaToAccount, encToCId, mkCCoin)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.State       (AddressLookupMode (Existing),
                                             CustomAddressType (ChangeAddr, UsedAddr),
                                             addWAddress, createAccount, createWallet,
                                             getAccountIds, getAccountMeta,
                                             getWalletAddresses,
                                             getWalletMetaIncludeUnready, getWalletPassLU,
                                             isCustomAddress, removeAccount,
                                             removeHistoryCache, removeTxMetas,
                                             removeWallet, setAccountMeta, setWalletMeta,
                                             setWalletPassLU, setWalletReady)
import           Pos.Wallet.Web.Tracking    (CAccModifier (..), CachedCAccModifier,
                                             fixCachedAccModifierFor,
                                             fixingCachedAccModifier, sortedInsertions)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                             getWalletAccountIds)


----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getWAddressBalance :: MonadWalletWebMode m => CWAddressMeta -> m Coin
getWAddressBalance addr = getBalance <=< decodeCTypeOrFail $ cwamId addr

-- BE CAREFUL: this function works for O(number of used and change addresses)
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

getAccount :: MonadWalletWebMode m => CachedCAccModifier -> AccountId -> m CAccount
getAccount accMod accId = do
    dbAddrs    <- getAccountAddrsOrThrow Existing accId
    let allAddrIds = gatherAddresses (camAddresses accMod) dbAddrs
    allAddrs <- mapM (getWAddress accMod) allAddrIds
    balance <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
               mapM (decodeCTypeOrFail . cadAmount) allAddrs
    meta <- getAccountMeta accId >>= maybeThrow noAccount
    pure $ CAccount (encodeCType accId) meta allAddrs balance
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId
    gatherAddresses addrModifier dbAddrs = do
        let memAddrs = sortedInsertions addrModifier
            relatedMemAddrs = filter ((== accId) . addrMetaToAccount) memAddrs
            -- @|relatedMemAddrs|@ is O(1) while @dbAddrs@ is large
            unknownMemAddrs = filter (`notElem` dbAddrs) relatedMemAddrs
        dbAddrs <> unknownMemAddrs

getAccountsIncludeUnready
    :: MonadWalletWebMode m
    => Bool -> Maybe (CId Wal) -> m [CAccount]
getAccountsIncludeUnready includeUnready mCAddr = do
    whenJust mCAddr $ \cAddr -> getWalletMetaIncludeUnready includeUnready cAddr `whenNothingM_` noWallet cAddr
    accIds <- maybe getAccountIds getWalletAccountIds mCAddr
    let groupedAccIds = fmap reverse $ HM.fromListWith mappend $
                        accIds <&> \acc -> (aiWId acc, [acc])
    concatForM (HM.toList groupedAccIds) $ \(wid, walAccIds) ->
         fixCachedAccModifierFor wid $ forM walAccIds . getAccount
  where
    noWallet cAddr = throwM . RequestError $
        -- TODO No WALLET with id ...
        -- dunno whether I can fix and not break compatible w/ daedalus
        sformat ("No account with id "%build%" found") cAddr

getAccounts
    :: MonadWalletWebMode m
    => Maybe (CId Wal) -> m [CAccount]
getAccounts = getAccountsIncludeUnready False

getWalletIncludeUnready :: MonadWalletWebMode m => Bool -> CId Wal -> m CWallet
getWalletIncludeUnready includeUnready cAddr = do
    meta       <- getWalletMetaIncludeUnready includeUnready cAddr >>= maybeThrow noWallet
    accounts   <- getAccountsIncludeUnready includeUnready (Just cAddr)
    let accountsNum = length accounts
    balance    <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
                     mapM (decodeCTypeOrFail . caAmount) accounts
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKById cAddr
    passLU     <- getWalletPassLU cAddr >>= maybeThrow noWallet
    pure $ CWallet cAddr meta accountsNum balance hasPass passLU
  where
    noWallet = RequestError $
        sformat ("No wallet with address "%build%" found") cAddr

getWallet :: MonadWalletWebMode m => CId Wal -> m CWallet
getWallet = getWalletIncludeUnready False

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

        cAccAddr <- genUniqueAddress addGenSeed passphrase accId
        addWAddress cAccAddr
        getWAddress accMod cAccAddr

newAccountIncludeUnready
    :: MonadWalletWebMode m
    => Bool -> AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccountIncludeUnready includeUnready addGenSeed passphrase CAccountInit {..} =
    fixCachedAccModifierFor caInitWId $ \accMod -> do
        -- check wallet exists
        _ <- getWalletIncludeUnready includeUnready caInitWId

        cAddr <- genUniqueAccountId addGenSeed caInitWId
        createAccount cAddr caInitMeta
        () <$ newAddress addGenSeed passphrase cAddr
        getAccount accMod cAddr

newAccount
    :: MonadWalletWebMode m
    => AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccount = newAccountIncludeUnready False

createWalletSafe
    :: MonadWalletWebMode m
    => CId Wal -> CWalletMeta -> Bool -> m CWallet
createWalletSafe cid wsMeta isReady = do
    -- Disallow duplicate wallets (including unready wallets)
    wSetExists <- isJust <$> getWalletMetaIncludeUnready True cid
    when wSetExists $
        throwM $ RequestError "Wallet with that mnemonics already exists"
    curTime <- liftIO getPOSIXTime
    createWallet cid wsMeta isReady curTime
    -- Return the newly created wallet irrespective of whether it's ready yet
    getWalletIncludeUnready True cid

markWalletReady
  :: MonadWalletWebMode m
  => CId Wal -> Bool -> m ()
markWalletReady cid isReady = do
    _ <- getWalletMetaIncludeUnready True cid >>= maybeThrow noWallet
    setWalletReady cid isReady
  where
    noWallet = RequestError $
        sformat ("No wallet with that id "%build%" found") cid


----------------------------------------------------------------------------
-- Deleters
----------------------------------------------------------------------------

deleteWallet :: MonadWalletWebMode m => CId Wal -> m ()
deleteWallet wid = do
    accounts <- getAccounts (Just wid)
    mapM_ (deleteAccount <=< decodeCTypeOrFail . caId) accounts
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
        newSK <- maybeThrow badPass =<< changeEncPassphrase oldPass newPass oldSK
        deleteSK oldPass
        addSecretKey newSK
        setWalletPassLU wid =<< liftIO getPOSIXTime
  where
    badPass = RequestError "Invalid old passphrase given"
    deleteSK passphrase = do
        let nice k = encToCId k == wid && isJust (checkPassMatches passphrase k)
        midx <- findIndex nice <$> getSecretKeysPlain
        idx  <- RequestError "No key with such address and pass found"
                `maybeThrow` midx
        deleteSecretKey (fromIntegral idx)
