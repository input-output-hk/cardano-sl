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
import qualified Data.HashMap.Strict as HM
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
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Aeson ()
import           Pos.Wallet.WalletMode (MonadBalances (..), WalletMempoolExt)
import           Pos.Wallet.Web.Account (AddrGenSeed, genUniqueAccountId, genUniqueAddress,
                                         getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccount (..), CAccountInit (..),
                                             CAccountMeta (..), CAddress (..), CCoin, CId,
                                             CWAddressMeta (..), CWallet (..), CWalletMeta (..),
                                             Wal, addrMetaToAccount, encToCId, mkCCoin)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (AddressLookupMode (Existing),
                                       CustomAddressType (ChangeAddr, UsedAddr), MonadWalletDB,
                                       MonadWalletDBRead, addWAddress, createAccount, createWallet,
                                       getAccountIds, getAccountMeta, getWalletAddresses,
                                       getWalletMetaIncludeUnready, getWalletPassLU,
                                       isCustomAddress, removeAccount, removeHistoryCache,
                                       removeTxMetas, removeWallet, setAccountMeta, setWalletMeta,
                                       setWalletPassLU, setWalletReady)
import           Pos.Wallet.Web.Tracking (BlockLockMode, CAccModifier (..), CachedCAccModifier,
                                          fixCachedAccModifierFor, fixingCachedAccModifier,
                                          sortedInsertions)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                      getWalletAccountIds)

type MonadWalletLogicRead ctx m =
    ( MonadIO m
    , MonadThrow m
    , WithLogger m
    , MonadRandom m
    , MonadSlots ctx m
    , MonadDBRead m
    , MonadBalances m
    , MonadWalletDBRead ctx m
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
    return $ CAddress aId (encodeCType balance) isUsed isChange

getAccount :: MonadWalletLogicRead ctx m => CachedCAccModifier -> AccountId -> m CAccount
getAccount accMod accId = do
    dbAddrs    <- getAccountAddrsOrThrow Existing accId
    let allAddrIds = gatherAddresses (camAddresses accMod) dbAddrs
    allAddrs <- mapM (getWAddress accMod) allAddrIds
    balance <- sumCCoin (map cadAmount allAddrs)
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
    :: MonadWalletLogicRead ctx m
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
getWallets = getWalletAddresses >>= mapM getWallet

----------------------------------------------------------------------------
-- Creators
----------------------------------------------------------------------------

newAddress
    :: MonadWalletLogic ctx m
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
    :: MonadWalletLogic ctx m
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
    fixingCachedAccModifier getAccount accId

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
