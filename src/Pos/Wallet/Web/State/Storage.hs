{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/Storage.hs
module Pos.Wallet.Web.State.Storage
       (
         WalletStorage (..)
       , AddressLookupMode (..)
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
       , createAccount
       , createWallet
       , addWAddress
       , addRemovedAccount
       , setAccountMeta
       , setWalletMeta
       , setWalletPassLU
       , setWalletSyncTip
       , setAccountHistory
       , getAccountHistory
       , addOnlyNewTxMeta
       , setAccountTransactionMeta
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

import           Control.Lens               (at, ix, makeClassy, makeLenses, (%=), (.=),
                                             (<<.=), (?=), _head)
import           Control.Monad.State.Class  (put)
import           Data.Default               (Default, def)
import qualified Data.HashMap.Strict        as HM
import           Data.SafeCopy              (base, deriveSafeCopySimple)

import           Pos.Client.Txp.History     (TxHistoryEntry)
import           Pos.Constants              (genesisHash)
import           Pos.Txp                    (Utxo)
import           Pos.Types                  (HeaderHash)
import           Pos.Util.BackupPhrase      (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes (AccountId (aiWId), Addr, CAccountMeta, CCoin,
                                             CHash, CId, CProfile, CTxId, CTxMeta,
                                             CUpdateInfo, CWAddressMeta (..),
                                             CWalletAssurance, CWalletMeta, PassPhraseLU,
                                             Wal, walletAddrMetaToAccount)

type TransactionHistory = HashMap CTxId CTxMeta

type CAddresses = HashSet CWAddressMeta

data WalletInfo = WalletInfo
    { _wiMeta         :: CWalletMeta
    , _wiPassphraseLU :: PassPhraseLU
    , _wiSyncTip      :: HeaderHash
    }

makeLenses ''WalletInfo

data AccountInfo = AccountInfo
    { _aiMeta            :: CAccountMeta
    , _aiAccounts        :: CAddresses
    , _aiRemovedAccounts :: CAddresses
    }

makeLenses ''AccountInfo

data WalletStorage = WalletStorage
    { _wsWalletInfos  :: !(HashMap (CId Wal) WalletInfo)
    , _wsAccountInfos :: !(HashMap AccountId AccountInfo)
    , _wsProfile      :: !CProfile
    , _wsReadyUpdates :: [CUpdateInfo]
    , _wsTxHistory    :: !(HashMap (CId Wal) TransactionHistory)
    , _wsHistoryCache :: !(HashMap (CId Wal) (HeaderHash, Utxo, [TxHistoryEntry]))
    }

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        { _wsWalletInfos  = mempty
        , _wsAccountInfos = mempty
        , _wsProfile      = def
        , _wsReadyUpdates = mempty
        , _wsTxHistory    = mempty
        , _wsHistoryCache = mempty
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

getProfile :: Query CProfile
getProfile = view wsProfile

setProfile :: CProfile -> Update ()
setProfile profile = wsProfile .= profile

getWAddressIds :: Query [AccountId]
getWAddressIds = HM.keys <$> view wsAccountInfos

getAccountMetas :: Query [CAccountMeta]
getAccountMetas = map (view aiMeta) . toList <$> view wsAccountInfos

getAccountMeta :: AccountId -> Query (Maybe CAccountMeta)
getAccountMeta cAddr = preview (wsAccountInfos . ix cAddr . aiMeta)

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = toList . fmap _wiMeta <$> view wsWalletInfos

getWalletMeta :: CId Wal -> Query (Maybe CWalletMeta)
getWalletMeta cAddr = preview (wsWalletInfos . ix cAddr . wiMeta)

getWalletPassLU :: CId Wal -> Query (Maybe PassPhraseLU)
getWalletPassLU cAddr = preview (wsWalletInfos . ix cAddr . wiPassphraseLU)

getWalletSyncTip :: CId Wal -> Query (Maybe HeaderHash)
getWalletSyncTip cAddr = preview (wsWalletInfos . ix cAddr . wiSyncTip)


getWalletAddresses :: Query [CId Wal]
getWalletAddresses = HM.keys <$> view wsWalletInfos

getAccountWAddresses :: AddressLookupMode
                  -> AccountId
                  -> Query (Maybe [CWAddressMeta])
getAccountWAddresses mode wAddr = do
    let fetch which = toList <<$>> preview (wsAccountInfos . ix wAddr . which)
    withAccLookupMode mode (fetch aiAccounts) (fetch aiRemovedAccounts)

doesWAddressExist :: AddressLookupMode -> CWAddressMeta -> Query Bool
doesWAddressExist mode accAddr@(walletAddrMetaToAccount -> wAddr) = do
    let exists :: Lens' AccountInfo CAddresses -> Query Any
        exists which =
            Any . isJust <$>
            preview (wsAccountInfos . ix wAddr . which . ix accAddr)
    getAny <$>
        withAccLookupMode mode (exists aiAccounts) (exists aiRemovedAccounts)

getTxMeta :: CId Wal -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cid ctxId = preview $ wsTxHistory . ix cid . ix ctxId

getAccountHistory :: AccountId -> Query (Maybe [CTxMeta])
getAccountHistory accId = toList <<$>> preview (wsTxHistory . ix (aiWId accId))

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: CId Wal -> Query (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache cWalId = view $ wsHistoryCache . at cWalId

createAccount :: AccountId -> CAccountMeta -> Update ()
createAccount accId wMeta = wsAccountInfos . at accId ?= AccountInfo wMeta mempty mempty

createWallet :: CId Wal -> CWalletMeta -> PassPhraseLU -> Update ()
createWallet cAddr wSMeta passLU = wsWalletInfos . at cAddr ?= WalletInfo wSMeta passLU genesisHash

addWAddress :: CWAddressMeta -> Update ()
addWAddress addr@CWAddressMeta{..} = do
    wsAccountInfos . ix (walletAddrMetaToAccount addr) . aiAccounts . at addr ?= ()

-- see also 'removeWAddress'
addRemovedAccount :: CWAddressMeta -> Update ()
addRemovedAccount addr@CWAddressMeta{..} = do
    let acc = walletAddrMetaToAccount addr
    wsAccountInfos . ix acc . aiAccounts . at addr .= Nothing
    wsAccountInfos . ix acc . aiRemovedAccounts . at addr ?= ()

setAccountMeta :: AccountId -> CAccountMeta -> Update ()
setAccountMeta cAddr wMeta = wsAccountInfos . ix cAddr . aiMeta .= wMeta

setWalletMeta :: CId Wal -> CWalletMeta -> Update ()
setWalletMeta cAddr wSMeta = wsWalletInfos . ix cAddr . wiMeta .= wSMeta

setWalletPassLU :: CId Wal -> PassPhraseLU -> Update ()
setWalletPassLU cAddr passLU = wsWalletInfos . ix cAddr . wiPassphraseLU .= passLU

setWalletSyncTip :: CId Wal -> HeaderHash -> Update ()
setWalletSyncTip cAddr hh = wsWalletInfos . ix cAddr . wiSyncTip .= hh

addAccountHistoryTx :: AccountId -> CTxId -> CTxMeta -> Update ()
addAccountHistoryTx accId ctxId ctxMeta =
    wsTxHistory . ix (aiWId accId) . at ctxId ?= ctxMeta

setAccountHistory :: AccountId -> [(CTxId, CTxMeta)] -> Update ()
setAccountHistory cAddr ctxs = mapM_ (uncurry $ addAccountHistoryTx cAddr) ctxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: CId Wal -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cWalId ctxId ctxMeta =
    wsTxHistory . ix cWalId . at ctxId %= Just . fromMaybe ctxMeta

-- NOTE: sets transaction meta only for transactions ids that are already seen
setAccountTransactionMeta :: AccountId -> CTxId -> CTxMeta -> Update ()
setAccountTransactionMeta accId ctxId ctxMeta =
    wsTxHistory . ix (aiWId accId) . at ctxId %= ($> ctxMeta)

removeWallet :: CId Wal -> Update ()
removeWallet cAddr = wsWalletInfos . at cAddr .= Nothing

removeAccount :: AccountId -> Update ()
removeAccount cAddr = wsAccountInfos . at cAddr .= Nothing

-- see also 'addRemovedAccount'
removeWAddress :: CWAddressMeta -> Update ()
removeWAddress accAddr@(walletAddrMetaToAccount -> wAddr) = do
    existed <- wsAccountInfos . ix wAddr . aiAccounts . at accAddr <<.= Nothing
    whenJust existed $ \_ ->
        wsAccountInfos . ix wAddr . aiRemovedAccounts . at accAddr ?= ()

totallyRemoveWAddress :: CWAddressMeta -> Update ()
totallyRemoveWAddress accAddr@(walletAddrMetaToAccount -> wAddr) = do
    wsAccountInfos . ix wAddr . aiAccounts . at accAddr .= Nothing
    wsAccountInfos . ix wAddr . aiRemovedAccounts . at accAddr .= Nothing

addUpdate :: CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= drop 1

testReset :: Update ()
testReset = put def

updateHistoryCache :: CId Wal -> HeaderHash -> Utxo -> [TxHistoryEntry] -> Update ()
updateHistoryCache cWalId cHash utxo cTxs =
    wsHistoryCache . at cWalId ?= (cHash, utxo, cTxs)

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
deriveSafeCopySimple 0 'base ''TxHistoryEntry
deriveSafeCopySimple 0 'base ''CTxMeta
deriveSafeCopySimple 0 'base ''CUpdateInfo
deriveSafeCopySimple 0 'base ''AddressLookupMode
deriveSafeCopySimple 0 'base ''WalletInfo
deriveSafeCopySimple 0 'base ''AccountInfo
deriveSafeCopySimple 0 'base ''WalletStorage
