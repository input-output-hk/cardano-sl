{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/Storage.hs
module Pos.Wallet.Web.State.Storage
       (
         WalletStorage (..)
       , AddressLookupMode (..)
       , CustomAddressType (..)
       , Query
       , Update
       , getProfile
       , setProfile
       , getWAddressIds
       , getAccountMetas
       , getAccountMeta
       , getWalletMetas
       , getWalletMeta
       , getWalletPassLU
       , getWalletSyncTip
       , getWalletAddresses
       , getAccountWAddresses
       , doesWAddressExist
       , getTxMeta
       , getUpdates
       , getNextUpdate
       , getHistoryCache
       , getCustomAddresses
       , getCustomAddress
       , addCustomAddress
       , removeCustomAddress
       , createAccount
       , createWallet
       , addWAddress
       , addRemovedAccount
       , setAccountMeta
       , setWalletMeta
       , setWalletPassLU
       , setWalletSyncTip
       , setWalletTxHistory
       , getWalletTxHistory
       , addOnlyNewTxMeta
       , setWalletTxMeta
       , removeWallet
       , removeAccount
       , removeWAddress
       , totallyRemoveWAddress
       , addUpdate
       , removeNextUpdate
       , testReset
       , updateHistoryCache
       ) where

import           Universum

import           Control.Lens               (at, ix, makeClassy, makeLenses, non', (%=),
                                             (.=), (<<.=), (?=), _Empty, _head)
import           Control.Monad.State.Class  (put)
import           Data.Default               (Default, def)
import qualified Data.HashMap.Strict        as HM
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import           Data.Time.Clock.POSIX      (POSIXTime)

import           Pos.Client.Txp.History     (TxHistoryEntry)
import           Pos.Constants              (genesisHash)
import           Pos.Core.Types             (Timestamp)
import           Pos.Txp                    (Utxo)
import           Pos.Types                  (HeaderHash)
import           Pos.Util.BackupPhrase      (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes (AccountId, Addr, CAccountMeta, CCoin, CHash,
                                             CId, CProfile, CTxId, CTxMeta, CUpdateInfo,
                                             CWAddressMeta (..), CWalletAssurance,
                                             CWalletMeta, PassPhraseLU, Wal,
                                             addrMetaToAccount)

type AddressSortingKey = Int

data AddressInfo = AddressInfo
    { adiCWAddressMeta :: !CWAddressMeta
    , adiSortingKey    :: !AddressSortingKey
    }

type CAddresses = HashMap (CId Addr) AddressInfo

data AccountInfo = AccountInfo
    { _aiMeta            :: !CAccountMeta
    , _aiAccounts        :: !CAddresses
    , _aiRemovedAccounts :: !CAddresses
    , _aiUnusedKey       :: !AddressSortingKey
    }

makeLenses ''AccountInfo

data WalletInfo = WalletInfo
    { _wiMeta         :: !CWalletMeta
    , _wiPassphraseLU :: !PassPhraseLU
    , _wiCreationTime :: !POSIXTime
    , _wiSyncTip      :: !HeaderHash
    }

makeLenses ''WalletInfo

type TransactionHistory = HashMap CTxId CTxMeta

-- | Maps addresses to their first occurrence in the blockchain
type CustomAddresses = HashMap (CId Addr) HeaderHash

data WalletStorage = WalletStorage
    { _wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _wsProfile         :: !CProfile
    , _wsReadyUpdates    :: [CUpdateInfo]
    , _wsTxHistory       :: !(HashMap (CId Wal) TransactionHistory)
    , _wsHistoryCache    :: !(HashMap (CId Wal) (HeaderHash, Utxo, [TxHistoryEntry]))
    , _wsUsedAddresses   :: !CustomAddresses
    , _wsChangeAddresses :: !CustomAddresses
    }

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        { _wsWalletInfos     = mempty
        , _wsAccountInfos    = mempty
        , _wsProfile         = def
        , _wsReadyUpdates    = mempty
        , _wsTxHistory       = mempty
        , _wsHistoryCache    = mempty
        , _wsUsedAddresses   = mempty
        , _wsChangeAddresses = mempty
        }

type Query a = forall m. (MonadReader WalletStorage m) => m a
type Update a = forall m. ({-MonadThrow m, -}MonadState WalletStorage m) => m a

-- | How to lookup addresses of account
data AddressLookupMode
    = Existing  -- ^ fetch only currently existing addresses
    | Deleted   -- ^ fetch only removed addresses
    | Ever      -- ^ fetch both existing and removed addresses

withAccLookupMode :: (Monad m, Monoid a) => AddressLookupMode -> m a -> m a -> m a
withAccLookupMode Existing existing _       = existing
withAccLookupMode Deleted  _        deleted = deleted
withAccLookupMode Ever     existing deleted = mappend <$> existing <*> deleted

-- | Specifies special category of addresses which are stored in base.
data CustomAddressType
    = UsedAddr
    | ChangeAddr

customAddressL :: CustomAddressType -> Lens' WalletStorage CustomAddresses
customAddressL UsedAddr   = wsUsedAddresses
customAddressL ChangeAddr = wsChangeAddresses

getProfile :: Query CProfile
getProfile = view wsProfile

setProfile :: CProfile -> Update ()
setProfile cProfile = wsProfile .= cProfile

getWAddressIds :: Query [AccountId]
getWAddressIds = HM.keys <$> view wsAccountInfos

getAccountMetas :: Query [CAccountMeta]
getAccountMetas = map (view aiMeta) . toList <$> view wsAccountInfos

getAccountMeta :: AccountId -> Query (Maybe CAccountMeta)
getAccountMeta accId = preview (wsAccountInfos . ix accId . aiMeta)

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = toList . fmap _wiMeta <$> view wsWalletInfos

getWalletMeta :: CId Wal -> Query (Maybe CWalletMeta)
getWalletMeta cWalId = preview (wsWalletInfos . ix cWalId . wiMeta)

getWalletPassLU :: CId Wal -> Query (Maybe PassPhraseLU)
getWalletPassLU cWalId = preview (wsWalletInfos . ix cWalId . wiPassphraseLU)

getWalletSyncTip :: CId Wal -> Query (Maybe HeaderHash)
getWalletSyncTip cWalId = preview (wsWalletInfos . ix cWalId . wiSyncTip)


getWalletAddresses :: Query [CId Wal]
getWalletAddresses =
    map fst . sortOn (view wiCreationTime . snd) . HM.toList <$>
    view wsWalletInfos

getAccountWAddresses :: AddressLookupMode
                  -> AccountId
                  -> Query (Maybe [CWAddressMeta])
getAccountWAddresses mode accId = do
    let fetch which = toList <<$>> preview (wsAccountInfos . ix accId . which)
    withAccLookupMode mode (fetch aiAccounts) (fetch aiRemovedAccounts)

doesWAddressExist :: AddressLookupMode -> CWAddressMeta -> Query Bool
doesWAddressExist mode accAddr@(addrMetaToAccount -> wAddr) = do
    let exists :: Lens' AccountInfo CAddresses -> Query Any
        exists which =
            Any . isJust <$>
            preview (wsAccountInfos . ix wAddr . which . ix accAddr)
    getAny <$>
        withAccLookupMode mode (exists aiAccounts) (exists aiRemovedAccounts)

getTxMeta :: CId Wal -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cid ctxId = preview $ wsTxHistory . ix cid . ix ctxId

getWalletTxHistory :: CId Wal -> Query (Maybe [CTxMeta])
getWalletTxHistory cWalId = toList <<$>> preview (wsTxHistory . ix cWalId)

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: CId Wal -> Query (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache cWalId = view $ wsHistoryCache . at cWalId

getCustomAddresses :: CustomAddressType -> Query [CId Addr]
getCustomAddresses t = HM.keys <$> view (customAddressL t)

getCustomAddress :: CustomAddressType -> CId Addr -> Query (Maybe HeaderHash)
getCustomAddress t addr = view $ customAddressL t . at addr

addCustomAddress :: CustomAddressType -> (CId Addr, HeaderHash) -> Update Bool
addCustomAddress t (addr, hh) = fmap isJust $ customAddressL t . at addr <<.= Just hh

removeCustomAddress :: CustomAddressType -> (CId Addr, HeaderHash) -> Update Bool
removeCustomAddress t (addr, hh) = do
    mhh' <- use $ customAddressL t . at addr
    let exists = mhh' == Just hh
    when exists $
        customAddressL t . at addr .= Nothing
    return exists

createAccount :: AccountId -> CAccountMeta -> Update ()
createAccount accId cAccMeta =
    wsAccountInfos . at accId %= Just . fromMaybe (AccountInfo cAccMeta mempty mempty)

createWallet :: CId Wal -> CWalletMeta -> POSIXTime -> Update ()
createWallet cWalId cWalMeta curTime = do
    let info = WalletInfo cWalMeta curTime curTime genesisHash
    wsWalletInfos . at cWalId %= (<|> Just info)

addWAddress :: CWAddressMeta -> Update ()
addWAddress addr@CWAddressMeta{..} = do
    wsAccountInfos . ix (addrMetaToAccount addr) . aiAccounts . at addr ?= ()

-- see also 'removeWAddress'
addRemovedAccount :: CWAddressMeta -> Update ()
addRemovedAccount addr@CWAddressMeta{..} = do
    let acc = addrMetaToAccount addr
    wsAccountInfos . ix acc . aiAccounts . at addr .= Nothing
    wsAccountInfos . ix acc . aiRemovedAccounts . at addr ?= ()

setAccountMeta :: AccountId -> CAccountMeta -> Update ()
setAccountMeta accId cAccMeta = wsAccountInfos . ix accId . aiMeta .= cAccMeta

setWalletMeta :: CId Wal -> CWalletMeta -> Update ()
setWalletMeta cWalId cWalMeta = wsWalletInfos . ix cWalId . wiMeta .= cWalMeta

setWalletPassLU :: CId Wal -> PassPhraseLU -> Update ()
setWalletPassLU cWalId passLU = wsWalletInfos . ix cWalId . wiPassphraseLU .= passLU

setWalletSyncTip :: CId Wal -> HeaderHash -> Update ()
setWalletSyncTip cWalId hh = wsWalletInfos . ix cWalId . wiSyncTip .= hh

addWalletTxHistory :: CId Wal -> CTxId -> CTxMeta -> Update ()
addWalletTxHistory cWalId cTxId cTxMeta =
    wsTxHistory . at cWalId . non' _Empty . at cTxId ?= cTxMeta

setWalletTxHistory :: CId Wal -> [(CTxId, CTxMeta)] -> Update ()
setWalletTxHistory cWalId cTxs = mapM_ (uncurry $ addWalletTxHistory cWalId) cTxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: CId Wal -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cWalId cTxId cTxMeta =
    -- Double nested HashMap update (if either or both of cWalId, cTxId don't exist, they will be created)
    wsTxHistory . at cWalId . non' _Empty . at cTxId %= Just . fromMaybe cTxMeta

-- NOTE: sets transaction meta only for transactions ids that are already seen
setWalletTxMeta :: CId Wal -> CTxId -> CTxMeta -> Update ()
setWalletTxMeta cWalId cTxId cTxMeta =
    wsTxHistory . ix cWalId . at cTxId %= ($> cTxMeta)

removeWallet :: CId Wal -> Update ()
removeWallet cWalId = wsWalletInfos . at cWalId .= Nothing

removeAccount :: AccountId -> Update ()
removeAccount accId = wsAccountInfos . at accId .= Nothing

-- see also 'addRemovedAccount'
removeWAddress :: CWAddressMeta -> Update ()
removeWAddress addr@(addrMetaToAccount -> accId) = do
    existed <- wsAccountInfos . ix accId . aiAccounts . at addr <<.= Nothing
    whenJust existed $ \_ ->
        wsAccountInfos . ix accId . aiRemovedAccounts . at addr ?= ()

totallyRemoveWAddress :: CWAddressMeta -> Update ()
totallyRemoveWAddress addr@(addrMetaToAccount -> accId) = do
    wsAccountInfos . ix accId . aiAccounts . at addr .= Nothing
    wsAccountInfos . ix accId . aiRemovedAccounts . at addr .= Nothing

addUpdate :: CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= drop 1

testReset :: Update ()
testReset = put def

updateHistoryCache :: CId Wal -> HeaderHash -> Utxo -> [TxHistoryEntry] -> Update ()
updateHistoryCache cWalId hh utxo cTxs =
    wsHistoryCache . at cWalId ?= (hh, utxo, cTxs)

deriveSafeCopySimple 0 'base ''CCoin
deriveSafeCopySimple 0 'base ''CProfile
deriveSafeCopySimple 0 'base ''CHash
deriveSafeCopySimple 0 'base ''CId
deriveSafeCopySimple 0 'base ''Wal
deriveSafeCopySimple 0 'base ''Addr
deriveSafeCopySimple 0 'base ''BackupPhrase
deriveSafeCopySimple 0 'base ''AccountId
deriveSafeCopySimple 0 'base ''CWAddressMeta
deriveSafeCopySimple 0 'base ''CWalletAssurance
deriveSafeCopySimple 0 'base ''CAccountMeta
deriveSafeCopySimple 0 'base ''CWalletMeta
deriveSafeCopySimple 0 'base ''CTxId
deriveSafeCopySimple 0 'base ''Timestamp
deriveSafeCopySimple 0 'base ''TxHistoryEntry
deriveSafeCopySimple 0 'base ''CTxMeta
deriveSafeCopySimple 0 'base ''CUpdateInfo
deriveSafeCopySimple 0 'base ''AddressLookupMode
deriveSafeCopySimple 0 'base ''CustomAddressType
deriveSafeCopySimple 0 'base ''AddressInfo
deriveSafeCopySimple 0 'base ''AccountInfo
deriveSafeCopySimple 0 'base ''WalletInfo
deriveSafeCopySimple 0 'base ''WalletStorage
