{-# LANGUAGE TypeFamilies #-}

-- | Wallets, accounts and addresses management logic

module Pos.Wallet.Web.Methods.Logic
       ( MonadWalletLogic
       , MonadWalletLogicRead

       , getWallet
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

import           Crypto.Random (MonadRandom)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Formatting (build, sformat, (%))
import           Servant.API.ContentTypes (NoContent (..))
import           System.Wlog (WithLogger)

import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead, addSecretKey,
                                        deleteSecretKeyBy)
import           Pos.Core (Coin, sumCoins, unsafeIntegerToCoin)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Crypto (PassPhrase, changeEncPassphrase, checkPassMatches, emptyPassphrase)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Slotting (MonadSlots)
import           Pos.Txp (MonadTxpMem)
import           Pos.Util (maybeThrow)
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Aeson ()
import           Pos.Wallet.WalletMode (MonadBalances (..), WalletMempoolExt)
import           Pos.Wallet.Web.Account (AddrGenSeed, genUniqueAccountId, genUniqueAddress,
                                         getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccount (..), CAccountInit (..),
                                             CAccountMeta (..), CAddress (..), CCoin, CId,
                                             CWAddressMeta (..), CWallet (..), CWalletMeta (..),
                                             Wal, encToCId, mkCCoin)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (AddressLookupMode (Existing),
                                       CustomAddressType (ChangeAddr, UsedAddr), MonadWalletDB,
                                       MonadWalletDBMempoolRead, addWAddress, createAccount,
                                       createWallet, getAccountMeta, getWalletAccountIds,
                                       getWalletIds, getWalletMetaIncludeUnready, getWalletPassLU,
                                       isCustomAddress, removeAccount, removeHistoryCache,
                                       removeTxMetas, removeWallet, setAccountMeta, setWalletMeta,
                                       setWalletPassLU, setWalletReady)
import           Pos.Wallet.Web.Tracking (BlockLockMode)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow)

type MonadWalletLogicRead ctx m =
    ( MonadIO m
    , MonadThrow m
    , WithLogger m
    , MonadRandom m
    , MonadSlots ctx m
    , MonadDBRead m
    , MonadBalances m
    , MonadWalletDBMempoolRead m
    , MonadKeysRead m
    , MonadTxpMem WalletMempoolExt ctx m  -- TODO: remove these two once 'fixingCachedAccModifier' becomes useless
    , BlockLockMode ctx m
    , HasConfiguration
    )

type MonadWalletLogic ctx m =
    ( MonadWalletLogicRead ctx m
    , MonadWalletDB ctx m
    , MonadKeys m
    )

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getWAddressBalance :: MonadWalletLogicRead ctx m => CWAddressMeta -> m Coin
getWAddressBalance addr =
    getBalance <=< decodeCTypeOrFail $ cwamId addr

sumCCoin :: MonadThrow m => [CCoin] -> m CCoin
sumCCoin ccoins = mkCCoin . unsafeIntegerToCoin . sumCoins <$> mapM decodeCTypeOrFail ccoins

-- BE CAREFUL: this function has complexity O(number of used and change addresses)
getWAddress
    :: MonadWalletLogicRead ctx m
    => CWAddressMeta -> m CAddress
getWAddress cAddr = do
    let aId = cwamId cAddr
    balance <- getWAddressBalance cAddr
    isUsed   <- isCustomAddress UsedAddr (cwamId cAddr)
    isChange <- isCustomAddress ChangeAddr (cwamId cAddr)
    return $ CAddress aId (encodeCType balance) isUsed isChange

getAccount :: MonadWalletLogicRead ctx m => AccountId -> m CAccount
getAccount accId = do
    allAddrIds <- getAccountAddrsOrThrow Existing accId
    allAddrs <- mapM getWAddress allAddrIds
    balance <- sumCCoin (map cadAmount allAddrs)
    meta <- getAccountMeta accId >>= maybeThrow noAccount
    pure $ CAccount (encodeCType accId) meta allAddrs balance
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId

getAccountsIncludeUnready
    :: MonadWalletLogicRead ctx m
    => Bool -> Maybe (CId Wal) -> m [CAccount]
getAccountsIncludeUnready includeUnready mCAddr = do
    whenJust mCAddr $ \cAddr ->
        getWalletMetaIncludeUnready includeUnready cAddr `whenNothingM_` noWSet cAddr
    -- TODO [CSM-515] remove mempool from getAccount
    maybe getAccountIds getWalletAccountIds mCAddr >>= mapM getAccount
  where
    getAccountIds = do
        walIds <- getWalletIds
        concatMapM getWalletAccountIds walIds
    noWSet cAddr = throwM . RequestError $
        sformat ("No account with id "%build%" found") cAddr

getAccounts
    :: MonadWalletLogicRead ctx m
    => Maybe (CId Wal) -> m [CAccount]
getAccounts = getAccountsIncludeUnready False

getWalletIncludeUnready :: MonadWalletLogicRead ctx m => Bool -> CId Wal -> m CWallet
getWalletIncludeUnready includeUnready cAddr = do
    meta       <- getWalletMetaIncludeUnready includeUnready cAddr >>= maybeThrow noWallet
    accounts   <- getAccountsIncludeUnready includeUnready (Just cAddr)
    let accountsNum = length accounts
    balance    <- sumCCoin (map caAmount accounts)
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKById cAddr
    passLU     <- getWalletPassLU cAddr >>= maybeThrow noWallet
    pure $ CWallet cAddr meta accountsNum balance hasPass passLU
  where
    noWallet = RequestError $
        sformat ("No wallet with address "%build%" found") cAddr

getWallet :: MonadWalletLogicRead ctx m => CId Wal -> m CWallet
getWallet = getWalletIncludeUnready False

getWallets :: MonadWalletLogicRead ctx m => m [CWallet]
getWallets = getWalletIds >>= mapM getWallet

----------------------------------------------------------------------------
-- Creators
----------------------------------------------------------------------------

newAddress
    :: MonadWalletLogic ctx m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newAddress addGenSeed passphrase accId = do
    -- check whether account exists
    _ <- getAccount accId

    cAccAddr <- genUniqueAddress addGenSeed passphrase accId
    addWAddress cAccAddr
    getWAddress cAccAddr

newAccountIncludeUnready
    :: MonadWalletLogic ctx m
    => Bool -> AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccountIncludeUnready includeUnready addGenSeed passphrase CAccountInit {..} = do
    -- check wallet exists
    _ <- getWalletIncludeUnready includeUnready caInitWId

    cAddr <- genUniqueAccountId addGenSeed caInitWId
    createAccount cAddr caInitMeta
    () <$ newAddress addGenSeed passphrase cAddr
    getAccount cAddr

newAccount
    :: MonadWalletLogic ctx m
    => AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccount = newAccountIncludeUnready False

createWalletSafe
    :: MonadWalletLogic ctx m
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
  :: MonadWalletLogic ctx m
  => CId Wal -> Bool -> m NoContent
markWalletReady cid isReady = do
    _ <- getWalletMetaIncludeUnready True cid >>= maybeThrow noWallet
    setWalletReady cid isReady
    return NoContent
  where
    noWallet = RequestError $
        sformat ("No wallet with that id "%build%" found") cid


----------------------------------------------------------------------------
-- Deleters
----------------------------------------------------------------------------

deleteWallet :: MonadWalletLogic ctx m => CId Wal -> m NoContent
deleteWallet wid = do
    accounts <- getAccounts (Just wid)
    mapM_ (deleteAccount <=< decodeCTypeOrFail . caId) accounts
    removeWallet wid
    removeTxMetas wid
    removeHistoryCache wid
    deleteSecretKeyBy ((== wid) . encToCId)
    return NoContent

deleteAccount :: MonadWalletLogic ctx m => AccountId -> m NoContent
deleteAccount aid = removeAccount aid $> NoContent

----------------------------------------------------------------------------
-- Modifiers
----------------------------------------------------------------------------

updateWallet :: MonadWalletLogic ctx m => CId Wal -> CWalletMeta -> m CWallet
updateWallet wId wMeta = do
    setWalletMeta wId wMeta
    getWallet wId

updateAccount :: MonadWalletLogic ctx m => AccountId -> CAccountMeta -> m CAccount
updateAccount accId wMeta = do
    setAccountMeta accId wMeta
    getAccount accId

changeWalletPassphrase
    :: MonadWalletLogic ctx m
    => CId Wal -> PassPhrase -> PassPhrase -> m NoContent
changeWalletPassphrase wid oldPass newPass = do
    oldSK <- getSKById wid

    unless (isJust $ checkPassMatches newPass oldSK) $ do
        newSK <- maybeThrow badPass =<< changeEncPassphrase oldPass newPass oldSK
        deleteSecretKeyBy ((== wid) . encToCId)
        addSecretKey newSK
        setWalletPassLU wid =<< liftIO getPOSIXTime
    return NoContent
  where
    badPass = RequestError "Invalid old passphrase given"
