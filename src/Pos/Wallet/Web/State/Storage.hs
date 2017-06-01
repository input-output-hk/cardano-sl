{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/Storage.hs
module Pos.Wallet.Web.State.Storage
       (
         WalletStorage (..)
       , AccountLookupMode (..)
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
import           Pos.Wallet.Web.ClientTypes (AccountId, Addr, CAccountMeta, CCoin, CHash,
                                             CId, CProfile, CTxId, CTxMeta, CUpdateInfo,
                                             CWAddressMeta, CWAddressMeta (..),
                                             CWalletAssurance, CWalletMeta, PassPhraseLU,
                                             Wal, walletAddrMetaToAccount)

type TransactionHistory = HashMap CTxId CTxMeta

type CAddresss = HashSet CWAddressMeta

data WalletSetInfo = WalletSetInfo
    { _wsiMeta         :: CWalletMeta
    , _wsiPassphraseLU :: PassPhraseLU
    , _wsiSyncTip      :: HeaderHash
    }

makeLenses ''WalletSetInfo

data WalletInfo = WalletInfo
    { _wiMeta            :: CAccountMeta
    , _wiAccounts        :: CAddresss
    , _wiRemovedAccounts :: CAddresss
    , _wiTxHistory       :: TransactionHistory
    }

makeLenses ''WalletInfo

data WalletStorage = WalletStorage
    { _wsWSetInfos    :: !(HashMap (CId Wal) WalletSetInfo)
    , _wsWalletInfos  :: !(HashMap AccountId WalletInfo)
    , _wsProfile      :: !CProfile
    , _wsReadyUpdates :: [CUpdateInfo]
    , _wsHistoryCache :: !(HashMap AccountId (HeaderHash, Utxo, [TxHistoryEntry]))
    }

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        { _wsWSetInfos    = mempty
        , _wsWalletInfos  = mempty
        , _wsProfile      = def
        , _wsReadyUpdates = mempty
        , _wsHistoryCache = mempty
        }

type Query a = forall m. (MonadReader WalletStorage m) => m a
type Update a = forall m. ({-MonadThrow m, -}MonadState WalletStorage m) => m a

-- | How to lookup accounts
data AccountLookupMode
    = Existing  -- ^ fetch only currently existing accounts
    | Deleted   -- ^ fetch only removed accounts
    | Ever      -- ^ fetch both existing and removed accounts

withAccLookupMode :: (Monad m, Monoid a) => AccountLookupMode -> m a -> m a -> m a
withAccLookupMode Existing existing _       = existing
withAccLookupMode Deleted  _        deleted = deleted
withAccLookupMode Ever     existing deleted = mappend <$> existing <*> deleted

getProfile :: Query CProfile
getProfile = view wsProfile

setProfile :: CProfile -> Update ()
setProfile profile = wsProfile .= profile

getWAddressIds :: Query [AccountId]
getWAddressIds = HM.keys <$> view wsWalletInfos

getAccountMetas :: Query [CAccountMeta]
getAccountMetas = map (view wiMeta) . toList <$> view wsWalletInfos

getAccountMeta :: AccountId -> Query (Maybe CAccountMeta)
getAccountMeta cAddr = preview (wsWalletInfos . ix cAddr . wiMeta)

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = toList . fmap _wsiMeta <$> view wsWSetInfos

getWalletMeta :: CId Wal -> Query (Maybe CWalletMeta)
getWalletMeta cAddr = preview (wsWSetInfos . ix cAddr . wsiMeta)

getWalletPassLU :: CId Wal -> Query (Maybe PassPhraseLU)
getWalletPassLU cAddr = preview (wsWSetInfos . ix cAddr . wsiPassphraseLU)

getWalletSyncTip :: CId Wal -> Query (Maybe HeaderHash)
getWalletSyncTip cAddr = preview (wsWSetInfos . ix cAddr . wsiSyncTip)


getWalletAddresses :: Query [CId Wal]
getWalletAddresses = HM.keys <$> view wsWSetInfos

getAccountWAddresses :: AccountLookupMode
                  -> AccountId
                  -> Query (Maybe [CWAddressMeta])
getAccountWAddresses mode wAddr = do
    let fetch which = toList <<$>> preview (wsWalletInfos . ix wAddr . which)
    withAccLookupMode mode (fetch wiAccounts) (fetch wiRemovedAccounts)

doesWAddressExist :: AccountLookupMode -> CWAddressMeta -> Query Bool
doesWAddressExist mode accAddr@(walletAddrMetaToAccount -> wAddr) = do
    let exists :: Lens' WalletInfo CAddresss -> Query Any
        exists which =
            Any . isJust <$>
            preview (wsWalletInfos . ix wAddr . which . ix accAddr)
    getAny <$>
        withAccLookupMode mode (exists wiAccounts) (exists wiRemovedAccounts)

getTxMeta :: AccountId -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cAddr ctxId = preview $ wsWalletInfos . ix cAddr . wiTxHistory . ix ctxId

getAccountHistory :: AccountId -> Query (Maybe [CTxMeta])
getAccountHistory cAddr = toList <<$>> preview (wsWalletInfos . ix cAddr . wiTxHistory)

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: AccountId -> Query (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache cAddr = view $ wsHistoryCache . at cAddr

createAccount :: AccountId -> CAccountMeta -> Update ()
createAccount cAddr wMeta = wsWalletInfos . at cAddr ?= WalletInfo wMeta mempty mempty mempty

createWallet :: CId Wal -> CWalletMeta -> PassPhraseLU -> Update ()
createWallet cAddr wSMeta passLU = wsWSetInfos . at cAddr ?= WalletSetInfo wSMeta passLU genesisHash

addWAddress :: CWAddressMeta -> Update ()
addWAddress addr@CWAddressMeta{..} = do
    wsWalletInfos . ix (walletAddrMetaToAccount addr) . wiAccounts . at addr ?= ()

-- see also 'removeWAddress'
addRemovedAccount :: CWAddressMeta -> Update ()
addRemovedAccount addr@CWAddressMeta{..} = do
    let acc = walletAddrMetaToAccount addr
    wsWalletInfos . ix acc . wiAccounts . at addr .= Nothing
    wsWalletInfos . ix acc . wiRemovedAccounts . at addr ?= ()

setAccountMeta :: AccountId -> CAccountMeta -> Update ()
setAccountMeta cAddr wMeta = wsWalletInfos . ix cAddr . wiMeta .= wMeta

setWalletMeta :: CId Wal -> CWalletMeta -> Update ()
setWalletMeta cAddr wSMeta = wsWSetInfos . ix cAddr . wsiMeta .= wSMeta

setWalletPassLU :: CId Wal -> PassPhraseLU -> Update ()
setWalletPassLU cAddr passLU = wsWSetInfos . ix cAddr . wsiPassphraseLU .= passLU

setWalletSyncTip :: CId Wal -> HeaderHash -> Update ()
setWalletSyncTip cAddr hh = wsWSetInfos . ix cAddr . wsiSyncTip .= hh

addAccountHistoryTx :: AccountId -> CTxId -> CTxMeta -> Update ()
addAccountHistoryTx cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId ?= ctxMeta

setAccountHistory :: AccountId -> [(CTxId, CTxMeta)] -> Update ()
setAccountHistory cAddr ctxs = mapM_ (uncurry $ addAccountHistoryTx cAddr) ctxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: AccountId -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId %= Just . fromMaybe ctxMeta

-- NOTE: sets transaction meta only for transactions ids that are already seen
setAccountTransactionMeta :: AccountId -> CTxId -> CTxMeta -> Update ()
setAccountTransactionMeta cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId %= ($> ctxMeta)

removeWallet :: CId Wal -> Update ()
removeWallet cAddr = wsWSetInfos . at cAddr .= Nothing

removeAccount :: AccountId -> Update ()
removeAccount cAddr = wsWalletInfos . at cAddr .= Nothing

-- see also 'addRemovedAccount'
removeWAddress :: CWAddressMeta -> Update ()
removeWAddress accAddr@(walletAddrMetaToAccount -> wAddr) = do
    existed <- wsWalletInfos . ix wAddr . wiAccounts . at accAddr <<.= Nothing
    whenJust existed $ \_ ->
        wsWalletInfos . ix wAddr . wiRemovedAccounts . at accAddr ?= ()

totallyRemoveWAddress :: CWAddressMeta -> Update ()
totallyRemoveWAddress accAddr@(walletAddrMetaToAccount -> wAddr) = do
    wsWalletInfos . ix wAddr . wiAccounts . at accAddr .= Nothing
    wsWalletInfos . ix wAddr . wiRemovedAccounts . at accAddr .= Nothing

addUpdate :: CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= drop 1

testReset :: Update ()
testReset = put def

updateHistoryCache :: AccountId -> HeaderHash -> Utxo -> [TxHistoryEntry] -> Update ()
updateHistoryCache cAddr cHash utxo cTxs =
    wsHistoryCache . at cAddr ?= (cHash, utxo, cTxs)

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
deriveSafeCopySimple 0 'base ''AccountLookupMode
deriveSafeCopySimple 0 'base ''WalletInfo
deriveSafeCopySimple 0 'base ''WalletSetInfo
deriveSafeCopySimple 0 'base ''WalletStorage
