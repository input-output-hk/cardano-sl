{-# LANGUAGE TypeFamilies #-}

-- | Wallets, accounts and addresses management logic

module Pos.Wallet.Web.Methods.Logic
       ( MonadWalletLogic
       , MonadWalletLogicRead

       , getWallet
       , getWallets
       , getWalletsWithInfo
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

        -- hack to support V1 Legacy handler
       , getWAddress
       , getMempoolSnapshot
       ) where

import           Universum

import           Crypto.Random (MonadRandom)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Formatting (build, sformat, (%))
import           Servant.API.ContentTypes (NoContent (..))
import           System.Wlog (WithLogger)

import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead, addSecretKey,
                                        deleteSecretKeyBy)
import           Pos.Core (Address, Coin, mkCoin, sumCoins, unsafeIntegerToCoin)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Crypto (PassPhrase, changeEncPassphrase, checkPassMatches, emptyPassphrase)
import           Pos.Infra.Slotting (MonadSlots)
import           Pos.Txp (GenericTxpLocalData, MonadTxpMem, TxAux, TxId, UndoMap,
                          applyUtxoModToAddrCoinMap, getLocalTxs, getLocalUndos, withTxpLocalData)
import           Pos.Util (maybeThrow)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Aeson ()
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Account (AddrGenSeed, findKey, genUniqueAccountId, genUniqueAddress,
                                         getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccount (..), CAccountInit (..),
                                             CAccountMeta (..), CAddress (..), CId, CWallet (..),
                                             CWalletMeta (..), Wal, encToCId, mkCCoin)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (AddressInfo (..),
                                       AddressLookupMode (Deleted, Ever, Existing),
                                       CustomAddressType (ChangeAddr, UsedAddr), WAddressMeta,
                                       WalletDbReader, WalletSnapshot, addWAddress, askWalletDB,
                                       askWalletSnapshot, createAccountWithAddress, createWallet,
                                       doesAccountExist, getAccountIds, getWalletAddresses,
                                       getWalletBalancesAndUtxo, getWalletMetaIncludeUnready,
                                       getWalletPassLU, getWalletSnapshot, isCustomAddress,
                                       removeAccount, removeWallet, setAccountMeta, setWalletMeta,
                                       setWalletPassLU, setWalletReady, wamAccount, wamAddress,
                                       wamWalletId)
import           Pos.Wallet.Web.State.Storage (WalletInfo (..), getWalletInfos)
import           Pos.Wallet.Web.Tracking (BlockLockMode, CAccModifier (..), CachedCAccModifier,
                                          sortedInsertions, txMempoolToModifier)
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Modifier (IndexedMapModifier (..))
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                      getAccountMetaOrThrow, getWalletAccountIds,
                                      getWalletAddrMetas)

type MonadWalletLogicRead ctx m =
    ( MonadIO m
    , MonadThrow m
    , WithLogger m
    , MonadRandom m
    , MonadSlots ctx m
    , MonadKeysRead m
    , MonadTxpMem WalletMempoolExt ctx m
    , BlockLockMode ctx m
    , HasConfiguration
    , WalletDbReader ctx m
    )

type MonadWalletLogic ctx m =
    ( MonadWalletLogicRead ctx m
    , MonadKeys m
    )

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getAccountMod
    :: MonadWalletLogicRead ctx m
    => WalletSnapshot
    -> CachedCAccModifier
    -> AccountId
    -> m CAccount
getAccountMod ws accMod accId = do
    dbAddrs    <- map adiWAddressMeta . sortOn adiSortingKey <$> getAccountAddrsOrThrow ws Existing accId
    let allAddrIds = gatherAddresses (camAddresses accMod) dbAddrs
        allAddrs = map (getWAddress ws accMod) allAddrIds
    balance <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
               mapM (decodeCTypeOrFail . cadAmount) allAddrs
    meta <- getAccountMetaOrThrow ws accId
    pure $ CAccount (encodeCType accId) meta allAddrs balance
  where
    gatherAddresses addrModifier dbAddrs = do
        let memAddrs = sortedInsertions addrModifier
            dbAddrsSet = S.fromList dbAddrs
            relatedMemAddrs = filter ((== accId) . view wamAccount) memAddrs
            unknownMemAddrs = filter (`S.notMember` dbAddrsSet) relatedMemAddrs
        dbAddrs <> unknownMemAddrs

getAccount
    :: MonadWalletLogicRead ctx m
    => NetworkMagic
    -> AccountId
    -> m CAccount
getAccount nm accId = do
    ws <- askWalletSnapshot
    mps <- withTxpLocalData getMempoolSnapshot
    accMod <- txMempoolToModifier ws mps . (eskToWalletDecrCredentials nm) =<< findKey nm accId
    getAccountMod ws accMod accId

getAccountsIncludeUnready
    :: MonadWalletLogicRead ctx m
    => NetworkMagic
    -> WalletSnapshot
    -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
    -> Bool -> Maybe (CId Wal) -> m [CAccount]
getAccountsIncludeUnready nm ws mps includeUnready mCAddr = do
    whenJust mCAddr $ \cAddr ->
      void $ maybeThrow (noWallet cAddr) $
        getWalletMetaIncludeUnready ws includeUnready cAddr
    let accIds = maybe (getAccountIds ws) (getWalletAccountIds ws) mCAddr
    let groupedAccIds = fmap reverse $ HM.fromListWith mappend $
                        accIds <&> \acc -> (aiWId acc, [acc])
    concatForM (HM.toList groupedAccIds) $ \(wid, walAccIds) -> do
      accMod <- txMempoolToModifier ws mps . (eskToWalletDecrCredentials nm) =<< findKey nm wid
      mapM (getAccountMod ws accMod) walAccIds
  where
    noWallet cAddr = RequestError $
        -- TODO No WALLET with id ...
        -- dunno whether I can fix and not break compatible w/ daedalus
        sformat ("No account with id "%build%" found") cAddr

getAccounts
    :: MonadWalletLogicRead ctx m
    => NetworkMagic
    -> Maybe (CId Wal)
    -> m [CAccount]
getAccounts nm mCAddr = do
    ws <- askWalletSnapshot
    mps <- withTxpLocalData getMempoolSnapshot
    getAccountsIncludeUnready nm ws mps False mCAddr

getWalletIncludeUnready :: MonadWalletLogicRead ctx m
                        => NetworkMagic
                        -> WalletSnapshot
                        -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
                        -> Bool -> CId Wal -> m CWallet
getWalletIncludeUnready nm ws mps includeUnready cAddr = do
    meta       <- maybeThrow noWallet $ getWalletMetaIncludeUnready ws includeUnready cAddr
    accounts   <- getAccountsIncludeUnready nm ws mps includeUnready (Just cAddr)
    let accountsNum = length accounts
    accMod     <- txMempoolToModifier ws mps . (eskToWalletDecrCredentials nm) =<< findKey nm cAddr
    balance    <- computeBalance accMod
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKById nm cAddr
    passLU     <- maybeThrow noWallet (getWalletPassLU ws cAddr)
    pure $ CWallet cAddr meta accountsNum balance hasPass passLU
  where
    computeBalance accMod = do
        let waddrIds = getWalletWAddrsWithMod ws Existing accMod cAddr
        let addrIds = map (view wamAddress) waddrIds
        let coins = getBalancesWithMod ws accMod addrIds
        pure . mkCCoin . unsafeIntegerToCoin . sumCoins $ coins

    noWallet = RequestError $
        sformat ("getWalletIncludeUnready: No wallet with address "%build%" found") cAddr

getWallet
    :: MonadWalletLogicRead ctx m
    => NetworkMagic
    -> CId Wal
    -> m CWallet
getWallet nm wid = do
    ws <- askWalletSnapshot
    mps <- withTxpLocalData getMempoolSnapshot
    getWalletIncludeUnready nm ws mps False wid

getWallets
    :: MonadWalletLogicRead ctx m
    => NetworkMagic
    -> m [CWallet]
getWallets nm = do
    ws <- askWalletSnapshot
    mps <- withTxpLocalData getMempoolSnapshot
    mapM (getWalletIncludeUnready nm ws mps False) (getWalletAddresses ws)

getWalletsWithInfo
    :: MonadWalletLogicRead ctx m
    => NetworkMagic
    -> WalletSnapshot
    -> m [(CWallet, WalletInfo)]
getWalletsWithInfo nm ws = do
    mps <- withTxpLocalData getMempoolSnapshot
    forM (getWalletInfos ws) $ \(cid, walInfo) -> do
        wal <- getWalletIncludeUnready nm ws mps False cid
        pure (wal, walInfo)

----------------------------------------------------------------------------
-- Creators
----------------------------------------------------------------------------

newAddress_
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> WalletSnapshot
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m WAddressMeta
newAddress_ nm ws addGenSeed passphrase accId = do
    -- check whether account exists
    let parentExists = doesAccountExist ws accId
    unless parentExists $ throwM noAccount

    -- XXX Transaction
    -- Make 'newAddress' generate a unique name internally
    cAccAddr <- genUniqueAddress nm ws addGenSeed passphrase accId
    db <- askWalletDB
    addWAddress db cAccAddr
    return cAccAddr
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId

newAddress
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newAddress nm addGenSeed passphrase accId = do
    mps <- withTxpLocalData getMempoolSnapshot
    ws <- askWalletSnapshot
    cwAddrMeta <- newAddress_ nm ws addGenSeed passphrase accId
    accMod <- txMempoolToModifier ws mps . (eskToWalletDecrCredentials nm) =<< findKey nm accId
    return $ getWAddress ws accMod cwAddrMeta

newAccountIncludeUnready
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> Bool
    -> AddrGenSeed
    -> PassPhrase
    -> CAccountInit
    -> m CAccount
newAccountIncludeUnready nm includeUnready addGenSeed passphrase CAccountInit {..} = do
    mps <- withTxpLocalData getMempoolSnapshot
    db <- askWalletDB
    ws <- getWalletSnapshot db
    -- TODO nclarke We read the mempool at this point to be consistent with the previous
    -- behaviour, but we may want to consider whether we should read it _after_ the
    -- account is created, since it's not used until we call 'getAccountMod'
    accMod <- txMempoolToModifier ws mps . (eskToWalletDecrCredentials nm) =<< findKey nm caInitWId
    -- check wallet exists
    _ <- getWalletIncludeUnready nm ws mps includeUnready caInitWId

    cAddr <- genUniqueAccountId ws addGenSeed caInitWId
    cAddrMeta <- genUniqueAddress nm ws addGenSeed passphrase cAddr

    createAccountWithAddress db cAddr caInitMeta cAddrMeta

    ws' <- askWalletSnapshot

    -- Re-read DB after the update.
    getAccountMod ws' accMod cAddr

newAccount
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> AddrGenSeed
    -> PassPhrase
    -> CAccountInit
    -> m CAccount
newAccount nm = newAccountIncludeUnready nm False

createWalletSafe
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> CId Wal
    -> CWalletMeta
    -> Bool
    -> m CWallet
createWalletSafe nm cid wsMeta isReady = do
    -- Disallow duplicate wallets (including unready wallets)
    db <- askWalletDB
    ws <- getWalletSnapshot db
    mps <- withTxpLocalData getMempoolSnapshot
    let wSetExists = isJust $ getWalletMetaIncludeUnready ws True cid
    when wSetExists $
        throwM $ RequestError "Wallet with that mnemonics already exists"
    curTime <- liftIO getPOSIXTime
    createWallet db cid wsMeta isReady curTime
    -- Return the newly created wallet irrespective of whether it's ready yet
    ws' <- getWalletSnapshot db
    getWalletIncludeUnready nm ws' mps True cid

markWalletReady
  :: MonadWalletLogic ctx m
  => CId Wal -> Bool -> m NoContent
markWalletReady cid isReady = do
    db <- askWalletDB
    ws <- getWalletSnapshot db
    _ <- maybeThrow noWallet $ getWalletMetaIncludeUnready ws True cid
    setWalletReady db cid isReady
    return NoContent
  where
    noWallet = RequestError $
        sformat ("markWalletReady: No wallet with that id "%build%" found") cid


----------------------------------------------------------------------------
-- Deleters
----------------------------------------------------------------------------

deleteWallet
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> CId Wal
    -> m NoContent
deleteWallet nm wid = do
    db <- askWalletDB
    removeWallet db wid
    deleteSecretKeyBy ((== wid) . (encToCId nm))
    return NoContent

deleteAccount :: MonadWalletLogicRead ctx m => AccountId -> m NoContent
deleteAccount accId = do
  db <- askWalletDB
  removeAccount db accId
  return NoContent

----------------------------------------------------------------------------
-- Modifiers
----------------------------------------------------------------------------

updateWallet
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> CId Wal
    -> CWalletMeta
    -> m CWallet
updateWallet nm wId wMeta = do
    db <- askWalletDB
    setWalletMeta db wId wMeta
    getWallet nm wId

updateAccount
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> AccountId
    -> CAccountMeta
    -> m CAccount
updateAccount nm accId wMeta = do
    db <- askWalletDB
    setAccountMeta db accId wMeta
    getAccount nm accId

changeWalletPassphrase
    :: MonadWalletLogic ctx m
    => NetworkMagic
    -> CId Wal
    -> PassPhrase
    -> PassPhrase
    -> m NoContent
changeWalletPassphrase nm wid oldPass newPass = do
    oldSK <- getSKById nm wid

    unless (isJust $ checkPassMatches newPass oldSK) $ do
        db <- askWalletDB
        newSK <- maybeThrow badPass =<< changeEncPassphrase oldPass newPass oldSK
        deleteSecretKeyBy ((== wid) . (encToCId nm))
        addSecretKey newSK
        setWalletPassLU db wid =<< liftIO getPOSIXTime
    return NoContent
  where
    badPass = RequestError "Invalid old passphrase given"

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | Get local transactions and undos from the mempool.
--   We define this function here rather than in 'Pos.DB.Txp.MemState.Class'
--   because it is less composable than the functions defined there - it
--   obfuscates the underlying structure. But hlint complains if we refuse
--   to unroll each of the uses in this module.
getMempoolSnapshot :: GenericTxpLocalData e -> STM ([(TxId, TxAux)], UndoMap)
getMempoolSnapshot txpData =  (,)
    <$> getLocalTxs txpData
    <*> getLocalUndos txpData

getBalanceWithMod :: WalletSnapshot -> CachedCAccModifier -> Address -> Coin
getBalanceWithMod ws accMod addr =
    let balancesAndUtxo = getWalletBalancesAndUtxo ws
    in  HM.lookupDefault (mkCoin 0) addr $
        flip applyUtxoModToAddrCoinMap balancesAndUtxo (camUtxo accMod)

getBalancesWithMod :: WalletSnapshot -> CachedCAccModifier -> [Address] -> [Coin]
getBalancesWithMod ws accMod addrs =
    let balancesAndUtxo = getWalletBalancesAndUtxo ws in
    let addrCoinsMap = applyUtxoModToAddrCoinMap (camUtxo accMod) balancesAndUtxo in
    let getBalance ad = HM.lookupDefault (mkCoin 0) ad addrCoinsMap in
    map getBalance addrs

getWAddressBalanceWithMod
    :: WalletSnapshot
    -> CachedCAccModifier
    -> WAddressMeta
    -> Coin
getWAddressBalanceWithMod ws accMod addr =
    getBalanceWithMod ws accMod (view wamAddress addr)

-- BE CAREFUL: this function has complexity O(number of used and change addresses)
getWAddress
    :: WalletSnapshot
    -> CachedCAccModifier
    -> WAddressMeta
    -> CAddress
getWAddress ws cachedAccModifier cAddr =
    let addr = view wamAddress cAddr
        aId = encodeCType addr
        balance = getWAddressBalanceWithMod ws cachedAccModifier cAddr

        getFlag customType accessMod =
            let checkDB = isCustomAddress ws customType addr
                checkMempool = elem addr . map (fst . fst) $
                               MM.insertions $ accessMod cachedAccModifier
            in (checkDB || checkMempool)
        isUsed   = getFlag UsedAddr camUsed
        isChange = getFlag ChangeAddr camChange
    in  CAddress aId (mkCCoin balance) isUsed isChange

getWalletWAddrsWithMod
    :: WalletSnapshot
    -> AddressLookupMode
    -> CachedCAccModifier
    -> CId Wal
    -> [WAddressMeta]
getWalletWAddrsWithMod ws mode cAccMod wid =
    let dbAddresses = getWalletAddrMetas ws mode wid
        addrMapMod = MM.filterWithKey (\k _ -> view wamWalletId k == wid) $ immModifier $ camAddresses cAccMod
    in  case mode of
            Existing ->
                filter (not . flip HM.member (MM.toHashMap addrMapMod)) dbAddresses ++
                map fst (MM.insertions addrMapMod)
            Deleted  -> dbAddresses ++ MM.deletions addrMapMod
            Ever     -> dbAddresses ++ HM.keys (MM.toHashMap addrMapMod)
