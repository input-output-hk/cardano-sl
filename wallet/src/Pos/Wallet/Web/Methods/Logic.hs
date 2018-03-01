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
       , newAddress_
       , markWalletReady

       , deleteWallet
       , deleteAccount

       , updateWallet
       , updateAccount
       , changeWalletPassphrase
       ) where

import           Universum

import qualified Data.HashMap.Strict          as HM
import           Data.List                    (findIndex)
import qualified Data.Set                     as S
import qualified Data.Map.Strict              as M
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Formatting                   (build, sformat, (%), int, shown, stext)
import           System.Wlog                  (logDebug, logInfo)

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.WalletBackup       ()
import           Pos.Core                     (Address, Coin, mkCoin, sumCoins,
                                               unsafeIntegerToCoin)
import           Pos.Crypto                   (PassPhrase, changeEncPassphrase,
                                               checkPassMatches, emptyPassphrase)
import           Pos.Txp                      (applyUtxoModToAddrCoinMap, Utxo, AddrCoinMap)
import           Pos.Util                     (maybeThrow)
import qualified Pos.Util.Modifier            as MM
import           Pos.Util.Servant             (encodeCType)
import           Pos.Wallet.KeyStorage        (addSecretKey, deleteSecretKey,
                                               getSecretKeysPlain)
import           Pos.Wallet.Web.Account       (AddrGenSeed, genUniqueAccountId,
                                               genUniqueAddress, getAddrIdx, getSKById)
import           Pos.Wallet.Web.ClientTypes   (AccountId (..), CAccount (..),
                                               CAccountInit (..), CAccountMeta (..),
                                               CAddress (..), CId, CWAddressMeta (..),
                                               CWallet (..), CWalletMeta (..), Wal,
                                               addrMetaToAccount, encToCId, mkCCoin)
import           Pos.Wallet.Web.Error         (WalletError (..))
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode, convertCIdTOAddr)
import           Pos.Wallet.Web.State         (AddressLookupMode (Existing),
                                               CustomAddressType (ChangeAddr, UsedAddr),
                                               addWAddress, createAccount, createWallet,
                                               getAccountIds,
                                               getWalletAddresses,
                                               getWalletMetaIncludeUnready,
                                               getWalletPassLU, isCustomAddress,
                                               removeAccount, removeHistoryCache,
                                               removeTxMetas, removeWallet,
                                               setAccountMeta, setWalletMeta,
                                               setWalletPassLU, setWalletReady)
import qualified Pos.Wallet.Web.State         as WS
import           Pos.Wallet.Web.State.Storage (NeedSorting (..), WalBalancesAndUtxo)
import           Pos.Wallet.Web.Tracking      (CAccModifier (..), CachedCAccModifier,
                                               fixCachedAccModifierFor,
                                               fixingCachedAccModifier, sortedInsertions)
import           Pos.Wallet.Web.Util          (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                               getWalletAccountIds, getAccountMetaOrThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getBalanceWithMod :: WalBalancesAndUtxo -> CachedCAccModifier -> Address -> Coin
getBalanceWithMod balancesAndUtxo accMod addr =
    fromMaybe (mkCoin 0) .
    HM.lookup addr $
    flip applyUtxoModToAddrCoinMap balancesAndUtxo (camUtxo accMod)

getWAddressBalanceWithMod
    :: MonadWalletWebMode m
    => WalBalancesAndUtxo
    -> CachedCAccModifier
    -> CWAddressMeta
    -> m Coin
getWAddressBalanceWithMod balancesAndUtxo accMod addr =
    getBalanceWithMod balancesAndUtxo accMod
        <$> convertCIdTOAddr (cwamId addr)

-- BE CAREFUL: this function works for O(number of used and change addresses)
getWAddress
    :: MonadWalletWebMode m
    => WalBalancesAndUtxo -> CachedCAccModifier -> CWAddressMeta -> m CAddress
getWAddress balancesAndUtxo cachedAccModifier cAddr = do
    let aId = cwamId cAddr
    balance <- getWAddressBalanceWithMod balancesAndUtxo cachedAccModifier cAddr

    let getFlag customType accessMod = do
            checkDB <- isCustomAddress customType (cwamId cAddr)
            let checkMempool = elem aId . map (fst . fst) . toList $
                               MM.insertions $ accessMod cachedAccModifier
            return (checkDB || checkMempool)
    isUsed   <- getFlag UsedAddr camUsed
    isChange <- getFlag ChangeAddr camChange
    return $ CAddress aId (mkCCoin balance) isUsed isChange

getAccountMod
    :: MonadWalletWebMode m
    => WalBalancesAndUtxo
    -> CachedCAccModifier
    -> AccountId
    -> m CAccount
getAccountMod balAndUtxo accMod accId = do
    logInfo $ sformat ("getAccountMod: Account " % build % " has accMod: " % build) accId accMod
    dbAddrs    <- getAccountAddrsOrThrow Existing (NeedSorting True) accId
    logInfo $ sformat ("getAccountMod: dbAddrs count: " % build) $ length dbAddrs
    let allAddrIds = gatherAddresses (camAddresses accMod) dbAddrs
    logInfo $ sformat ("getAccountMod: allAddrIds (dbAddrd + unknownMemAddrs) count: " % build)
            $ length allAddrIds
    logDebug "getAccountMod: gathering info about addresses.."
    allAddrs <- mapM (getWAddress balAndUtxo accMod) allAddrIds
    logDebug "getAccountMod: info about addresses gathered"
    balance <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
               mapM (decodeCTypeOrFail . cadAmount) allAddrs
    logInfo $ sformat ("getAccountMod: Account " % build % " has balance: " % build) accId balance
    meta <- getAccountMetaOrThrow accId
    pure $ CAccount (encodeCType accId) meta allAddrs balance
  where
    gatherAddresses addrModifier dbAddrs = do
        let memAddrs = sortedInsertions addrModifier
            dbAddrsSet = S.fromList dbAddrs
            relatedMemAddrs = filter ((== accId) . addrMetaToAccount) memAddrs
            unknownMemAddrs = filter (`S.notMember` dbAddrsSet) relatedMemAddrs
        dbAddrs <> unknownMemAddrs

getAccount :: MonadWalletWebMode m => AccountId -> m CAccount
getAccount accId = do
    balAndUtxo <- WS.getWalletBalancesAndUtxo
    fixingCachedAccModifier (getAccountMod balAndUtxo) accId

getAccountsIncludeUnready
    :: MonadWalletWebMode m
    => Bool -> Maybe (CId Wal) -> m [CAccount]
getAccountsIncludeUnready includeUnready mCAddr = do
    whenJust mCAddr $ \cAddr -> getWalletMetaIncludeUnready includeUnready cAddr `whenNothingM_` noWallet cAddr
    accIds <- maybe getAccountIds getWalletAccountIds mCAddr
    logInfo $ sformat ("getAccountsIncludeUnready: Computing balance for " % shown) accIds
    let groupedAccIds = fmap reverse $ HM.fromListWith mappend $
                        accIds <&> \acc -> (aiWId acc, [acc])
    logInfo $ sformat ("getAccountsIncludeUnready: Grouped AccountId(s) " % shown) groupedAccIds
    balAndUtxo <- WS.getWalletBalancesAndUtxo
    logInfo $ sformat ("getAccountsIncludeUnready: " % stext) (renderUtxo    . snd $ balAndUtxo)
    logInfo $ sformat ("getAccountsIncludeUnready: " % stext) (renderBalance . fst $ balAndUtxo)
    cAccounts <- concatForM (HM.toList groupedAccIds) $ \(wid, walAccIds) ->
                     fixCachedAccModifierFor wid $ \accMod ->
                         mapM (getAccountMod balAndUtxo accMod) walAccIds
    forM_ cAccounts $ \CAccount{..} -> logInfo $ sformat ("Account " % build % " has balance " % build) caId caAmount
    pure cAccounts
  where
    noWallet cAddr = throwM . RequestError $
        -- TODO No WALLET with id ...
        -- dunno whether I can fix and not break compatible w/ daedalus
        sformat ("No account with id "%build%" found") cAddr

    renderBalance :: AddrCoinMap -> Text
    renderBalance coinMap =
        sformat ("Entries in the AddrCoinMap: " % int) (HM.size coinMap)

    renderUtxo :: Utxo -> Text
    renderUtxo utxo =
        sformat ("Entries in the Utxo: " % int) (M.size utxo)


getAccounts
    :: MonadWalletWebMode m
    => Maybe (CId Wal) -> m [CAccount]
getAccounts = getAccountsIncludeUnready False

getWalletIncludeUnready :: MonadWalletWebMode m => Bool -> CId Wal -> m CWallet
getWalletIncludeUnready includeUnready cAddr = do
    meta       <- getWalletMetaIncludeUnready includeUnready cAddr >>= maybeThrow noWallet
    accounts   <- getAccountsIncludeUnready includeUnready (Just cAddr)
    let accountsNum = length accounts
    logInfo $ sformat ("getWalletIncludeUnready: Computing balance out of " % int % " accounts..") accountsNum
    balance    <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
                     mapM (decodeCTypeOrFail . caAmount) accounts
    logInfo $ sformat ("getWalletIncludeUnready: Balance is " % build % ".") balance
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

newAddress_
    :: MonadWalletWebMode m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CWAddressMeta
newAddress_ addGenSeed passphrase accId = do
    -- check whether account exists
    parentExists <- WS.doesAccountExist accId
    unless parentExists $ throwM noAccount

    cwAddrMeta <- genUniqueAddress addGenSeed passphrase accId
    addWAddress cwAddrMeta
    return cwAddrMeta
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId

newAddress
    :: MonadWalletWebMode m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newAddress addGenSeed passphrase accId = do
    balAndUtxo <- WS.getWalletBalancesAndUtxo
    cwAddrMeta <- newAddress_ addGenSeed passphrase accId
    fixCachedAccModifierFor accId $ \accMod -> do
        getWAddress balAndUtxo accMod cwAddrMeta

newAccountIncludeUnready
    :: MonadWalletWebMode m
    => Bool -> AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccountIncludeUnready includeUnready addGenSeed passphrase CAccountInit {..} = do
    balAndUtxo <- WS.getWalletBalancesAndUtxo
    fixCachedAccModifierFor caInitWId $ \accMod -> do
        -- check wallet exists
        _ <- getWalletIncludeUnready includeUnready caInitWId

        cAddr <- genUniqueAccountId addGenSeed caInitWId
        createAccount cAddr caInitMeta
        () <$ newAddress addGenSeed passphrase cAddr
        getAccountMod balAndUtxo accMod cAddr

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
    getAccount accId

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
