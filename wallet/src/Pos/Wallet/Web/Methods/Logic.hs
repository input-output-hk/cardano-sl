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

       , doesWalletExist
       , isWalletExternal
       , createWalletSafe
       , newAccount
       , newAccountIncludeUnready
       , newExternalAccount
       , newExternalAccountIncludeUnready
       , newAddress
       , newAddress_
       , storeNewAddress
       , markWalletReady

       , deleteWallet
       , deleteExternalWallet
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

import           Pos.Chain.Txp (TxAux, TxId, UndoMap, applyUtxoModToAddrCoinMap)
import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead,
                     addSecretKey, deletePublicKeyBy, deleteSecretKeyBy)
import           Pos.Core (Address, Coin, makePubKeyAddressBoot, mkCoin,
                     sumCoins, unsafeIntegerToCoin)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Crypto (PassPhrase, PublicKey, changeEncPassphrase,
                     checkPassMatches, emptyPassphrase, firstHardened)
import           Pos.DB.Txp (GenericTxpLocalData, MonadTxpMem, getLocalTxs,
                     getLocalUndos, withTxpLocalData)
import           Pos.Infra.Slotting (MonadSlots)
import           Pos.Util (maybeThrow)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Aeson ()
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Account (AddrGenSeed, GenSeed (DeterminedSeed),
                     findKey, genUniqueAccountId, genUniqueAddress,
                     genUniqueAddressIndex, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccount (..),
                     CAccountInit (..), CAccountMeta (..), CAddress (..), CId,
                     CWallet (..), CWalletMeta (..), Wal, encToCId, mkCCoin)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (AddressInfo (..),
                     AddressLookupMode (Deleted, Ever, Existing),
                     CustomAddressType (ChangeAddr, UsedAddr),
                     WAddressMeta (..), WalletDB, WalletDbReader,
                     WalletSnapshot, addWAddress, askWalletDB,
                     askWalletSnapshot, createAccountWithAddress,
                     createAccountWithoutAddresses, createWallet,
                     doesAccountExist, getAccountIds, getWalletAddresses,
                     getWalletBalancesAndUtxo, getWalletMetaIncludeUnready,
                     getWalletPassLU, getWalletSnapshot, isCustomAddress,
                     removeAccount, removeWallet, setAccountMeta,
                     setWalletMeta, setWalletPassLU, setWalletReady,
                     wamAccount, wamAddress, wamWalletId, _wamAddressIndex)
import           Pos.Wallet.Web.State.Storage (WalletInfo (..), getWalletInfos)
import           Pos.Wallet.Web.Tracking (BlockLockMode, CAccModifier (..),
                     CachedCAccModifier, sortedInsertions, txMempoolToModifier)
import           Pos.Wallet.Web.Tracking.Decrypt (keyToWalletDecrCredentials)
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

getAccount :: MonadWalletLogicRead ctx m => AccountId -> m CAccount
getAccount accId = do
    (mps, _, ws) <- getSnapshots
    accMod <- txMempoolToModifier ws mps . keyToWalletDecrCredentials =<< findKey accId
    getAccountMod ws accMod accId

getAccountsIncludeUnready
    :: MonadWalletLogicRead ctx m
    => WalletSnapshot
    -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
    -> Bool -> Maybe (CId Wal) -> m [CAccount]
getAccountsIncludeUnready ws mps includeUnready mCAddr = do
    whenJust mCAddr $ \cAddr ->
      void $ maybeThrow (noSuchWallet cAddr) $
        getWalletMetaIncludeUnready ws includeUnready cAddr
    let accIds = maybe (getAccountIds ws) (getWalletAccountIds ws) mCAddr
    let groupedAccIds = fmap reverse $ HM.fromListWith mappend $
                        accIds <&> \acc -> (aiWId acc, [acc])
    concatForM (HM.toList groupedAccIds) $ \(wid, walAccIds) -> do
      accMod <- txMempoolToModifier ws mps . keyToWalletDecrCredentials =<< findKey wid
      mapM (getAccountMod ws accMod) walAccIds
  where
    noSuchWallet cAddr = NoSuchWalletError $ sformat build cAddr

getAccounts
    :: MonadWalletLogicRead ctx m
    => Maybe (CId Wal) -> m [CAccount]
getAccounts mCAddr = do
    (mps, _, ws) <- getSnapshots
    getAccountsIncludeUnready ws mps False mCAddr

getWalletIncludeUnready :: MonadWalletLogicRead ctx m
                        => WalletSnapshot
                        -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
                        -> Bool -> CId Wal -> m CWallet
getWalletIncludeUnready ws mps includeUnready cAddr = do
    meta     <- maybeThrow noSuchWallet $ getWalletMetaIncludeUnready ws includeUnready cAddr
    accounts <- getAccountsIncludeUnready ws mps includeUnready (Just cAddr)
    let accountsNum = length accounts
    key      <- findKey cAddr
    accMod   <- txMempoolToModifier ws mps . keyToWalletDecrCredentials $ key
    balance  <- computeBalance accMod
    hasPass  <- getSKById cAddr >>= \case
                    Nothing -> return False -- No secret key, it's external wallet, so no password.
                    Just sk -> return $ isNothing . checkPassMatches emptyPassphrase $ sk
    passLU   <- maybeThrow noSuchWallet (getWalletPassLU ws cAddr)
    pure $ CWallet cAddr meta accountsNum balance hasPass passLU
  where
    computeBalance accMod = do
        let waddrIds = getWalletWAddrsWithMod ws Existing accMod cAddr
        let addrIds = map (view wamAddress) waddrIds
        let coins = getBalancesWithMod ws accMod addrIds
        pure . mkCCoin . unsafeIntegerToCoin . sumCoins $ coins
    noSuchWallet = NoSuchWalletError $ sformat build cAddr

getWallet :: MonadWalletLogicRead ctx m => CId Wal -> m CWallet
getWallet wid = do
    (mps, _, ws) <- getSnapshots
    getWalletIncludeUnready ws mps False wid

getWallets ::  MonadWalletLogicRead ctx m => m [CWallet]
getWallets = do
    (mps, _, ws) <- getSnapshots
    mapM (getWalletIncludeUnready ws mps False) (getWalletAddresses ws)

getWalletsWithInfo
    :: MonadWalletLogicRead ctx m
    => WalletSnapshot
    -> m [(CWallet, WalletInfo)]
getWalletsWithInfo ws = do
    mps <- withTxpLocalData getMempoolSnapshot
    forM (getWalletInfos ws) $ \(cid, walInfo) -> do
        wal <- getWalletIncludeUnready ws mps False cid
        pure (wal, walInfo)

----------------------------------------------------------------------------
-- Creators
----------------------------------------------------------------------------

newAddress_
    :: MonadWalletLogic ctx m
    => WalletSnapshot
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m WAddressMeta
newAddress_ ws addGenSeed passphrase accId = do
    -- check whether account exists
    let parentExists = doesAccountExist ws accId
    unless parentExists $ throwM noAccount

    -- XXX Transaction
    -- Make 'newAddress' generate a unique name internally
    cAccAddr <- genUniqueAddress ws addGenSeed passphrase accId
    db <- askWalletDB
    addWAddress db cAccAddr
    return cAccAddr
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId

newAddress
    :: MonadWalletLogic ctx m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newAddress addGenSeed passphrase accId = do
    (mps, _, ws) <- getSnapshots
    cwAddrMeta <- newAddress_ ws addGenSeed passphrase accId
    accMod <- txMempoolToModifier ws mps . keyToWalletDecrCredentials =<< findKey accId
    return $ getWAddress ws accMod cwAddrMeta

storeNewAddress
    :: MonadWalletLogic ctx m
    => AccountId
    -> Address
    -> m NoContent
storeNewAddress accId newAddr = do
    (_, db, ws) <- getSnapshots

    -- Check whether this account exists in external wallet.
    let parentExists = doesAccountExist ws accId
    unless parentExists $ throwM noAccount

    let walletId      = aiWId accId
        accIndex      = aiIndex accId
        fakeAddrIndex = 0
        wAddrMeta     = WAddressMeta walletId accIndex fakeAddrIndex newAddr

    -- Address already exists, but we have to generate unique index for it.
    realAddrIndex <- genUniqueAddressIndex ws wAddrMeta
    let realWAddrMeta = wAddrMeta { _wamAddressIndex = realAddrIndex }
    addWAddress db realWAddrMeta
    return NoContent
  where
    noAccount = RequestError $ sformat ("storeNewAddress: no account with id "%build%" found")
                                       accId

newAccountIncludeUnready
    :: MonadWalletLogic ctx m
    => Bool -> AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccountIncludeUnready includeUnready addGenSeed passphrase CAccountInit {..} = do
    (mps, db, ws) <- getSnapshots
    -- TODO nclarke We read the mempool at this point to be consistent with the previous
    -- behaviour, but we may want to consider whether we should read it _after_ the
    -- account is created, since it's not used until we call 'getAccountMod'
    accMod <- txMempoolToModifier ws mps . keyToWalletDecrCredentials =<< findKey caInitWId
    -- check wallet exists
    _ <- getWalletIncludeUnready ws mps includeUnready caInitWId

    cAddr <- genUniqueAccountId ws addGenSeed caInitWId
    cAddrMeta <- genUniqueAddress ws addGenSeed passphrase cAddr

    createAccountWithAddress db cAddr caInitMeta cAddrMeta

    ws' <- askWalletSnapshot

    -- Re-read DB after the update.
    getAccountMod ws' accMod cAddr

newAccount
    :: MonadWalletLogic ctx m
    => AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccount = newAccountIncludeUnready False

newExternalAccountIncludeUnready
    :: MonadWalletLogic ctx m
    => Bool
    -> CAccountInit
    -> m CAccount
newExternalAccountIncludeUnready includeUnready (CAccountInit accountMeta walletId) = do
    -- This is an account for external wallet, so there's no 'AddrGenSeed'
    -- (because addresses for external wallets can be generated only on device)
    -- and 'PassPhrase' (because external wallet doesn't have spending password).
    (mps, db, ws) <- getSnapshots
    accModifier <- txMempoolToModifier ws mps . keyToWalletDecrCredentials =<< findKey walletId
    -- Check that corresponding external wallet exists.
    void $ getWalletIncludeUnready ws mps includeUnready walletId

    accountId <- genUniqueAccountId ws (DeterminedSeed firstHardened) walletId

    createAccountWithoutAddresses db accountId accountMeta

    -- Re-read DB after the update.
    ws' <- askWalletSnapshot
    getAccountMod ws' accModifier accountId

-- | New account for external wallet.
newExternalAccount
    :: MonadWalletLogic ctx m
    => CAccountInit
    -> m CAccount
newExternalAccount = newExternalAccountIncludeUnready False

doesWalletExist
    :: MonadWalletLogic ctx m
    => CId Wal -> m Bool
doesWalletExist wId = do
    (_, _, ws) <- getSnapshots
    return $ isJust $ getWalletMetaIncludeUnready ws True wId

-- | Node stores public key to identify external wallets,
-- it is a 'Left'-variant here.
isWalletExternal
    :: MonadWalletLogicRead ctx m
    => CId Wal -> m Bool
isWalletExternal wId = findKey wId >>= pure . isLeft

createWalletSafe
    :: MonadWalletLogic ctx m
    => CId Wal -> CWalletMeta -> Bool -> m CWallet
createWalletSafe cid wsMeta isReady = do
    -- Disallow duplicate wallets (including unready wallets)
    (mps, db, ws) <- getSnapshots
    let wSetExists = isJust $ getWalletMetaIncludeUnready ws True cid
    when wSetExists $ throwM (DuplicateWalletError $ sformat build cid)
    curTime <- liftIO getPOSIXTime
    createWallet db cid wsMeta isReady curTime
    -- Return the newly created wallet irrespective of whether it's ready yet
    ws' <- getWalletSnapshot db
    getWalletIncludeUnready ws' mps True cid

markWalletReady
  :: MonadWalletLogic ctx m
  => CId Wal -> Bool -> m NoContent
markWalletReady cid isReady = do
    (_, db, ws) <- getSnapshots
    _ <- maybeThrow noSuchWallet $ getWalletMetaIncludeUnready ws True cid
    setWalletReady db cid isReady
    return NoContent
  where
    noSuchWallet = NoSuchWalletError $ sformat build cid

----------------------------------------------------------------------------
-- Deleters
----------------------------------------------------------------------------

deleteWallet :: MonadWalletLogic ctx m => CId Wal -> m NoContent
deleteWallet wid = do
    db <- askWalletDB
    removeWallet db wid
    deleteSecretKeyBy ((== wid) . encToCId)
    return NoContent

deleteExternalWallet :: MonadWalletLogic ctx m => PublicKey -> m NoContent
deleteExternalWallet publicKey = do
    let walletId = encodeCType . makePubKeyAddressBoot $ publicKey
    db <- askWalletDB
    removeWallet db walletId
    -- There's no secret key for an external wallet.
    deletePublicKeyBy (== publicKey)
    return NoContent

deleteAccount :: MonadWalletLogicRead ctx m => AccountId -> m NoContent
deleteAccount accId = do
  db <- askWalletDB
  removeAccount db accId
  return NoContent

----------------------------------------------------------------------------
-- Modifiers
----------------------------------------------------------------------------

updateWallet :: MonadWalletLogic ctx m => CId Wal -> CWalletMeta -> m CWallet
updateWallet wId wMeta = do
    db <- askWalletDB
    setWalletMeta db wId wMeta
    getWallet wId

updateAccount :: MonadWalletLogic ctx m => AccountId -> CAccountMeta -> m CAccount
updateAccount accId wMeta = do
    db <- askWalletDB
    setAccountMeta db accId wMeta
    getAccount accId

changeWalletPassphrase
    :: MonadWalletLogic ctx m
    => CId Wal -> PassPhrase -> PassPhrase -> m NoContent
changeWalletPassphrase wid oldPass newPass = do
    -- Spending password is related to internal wallet only,
    -- so secret key must be here.
    oldSK <- maybeThrow noSuchWallet =<< getSKById wid

    unless (isJust $ checkPassMatches newPass oldSK) $ do
        db <- askWalletDB
        newSK <- maybeThrow badPass =<< changeEncPassphrase oldPass newPass oldSK
        deleteSecretKeyBy ((== wid) . encToCId)
        addSecretKey newSK
        setWalletPassLU db wid =<< liftIO getPOSIXTime
    return NoContent
  where
    badPass = RequestError "Invalid old passphrase given"
    noSuchWallet = NoSuchWalletError $ sformat build wid

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

getSnapshots
    :: MonadWalletLogicRead ctx m
    => m ( ([(TxId, TxAux)], UndoMap)
         , WalletDB
         , WalletSnapshot
         )
getSnapshots = do
    mps <- withTxpLocalData getMempoolSnapshot
    db  <- askWalletDB
    ws  <- getWalletSnapshot db
    return (mps, db, ws)
