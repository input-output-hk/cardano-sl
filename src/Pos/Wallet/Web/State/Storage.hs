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
       , getAccountIds
       , getWalletMetas
       , getWalletMeta
       , getWSetMetas
       , getWSetMeta
       , getWSetPassLU
       , getWSetSyncTip
       , getWSetAddresses
       , getWalletAccounts
       , doesAccountExist
       , getTxMeta
       , getUpdates
       , getNextUpdate
       , getHistoryCache
       , createWallet
       , createWSet
       , addAccount
       , addRemovedAccount
       , setWalletMeta
       , setWSetMeta
       , setWSetPassLU
       , setWSetSyncTip
       , setWalletHistory
       , getWalletHistory
       , addOnlyNewTxMeta
       , setWalletTransactionMeta
       , removeWSet
       , removeWallet
       , removeAccount
       , totallyRemoveAccount
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
import           Pos.Wallet.Web.ClientTypes (Addr, CWAddressMeta, CWAddressMeta (..),
                                             CId, CCoin, CHash, CProfile, CTxId,
                                             CTxMeta, CUpdateInfo, CAccountMeta,
                                             CWalletAssurance, CWalletMeta,
                                             PassPhraseLU, WS, AccountId,
                                             walletAddrMetaToAccount)

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
    { _wsWSetInfos    :: !(HashMap (CId WS) WalletSetInfo)
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

getAccountIds :: Query [AccountId]
getAccountIds = HM.keys <$> view wsWalletInfos

getWalletMetas :: Query [CAccountMeta]
getWalletMetas = map (view wiMeta) . toList <$> view wsWalletInfos

getWalletMeta :: AccountId -> Query (Maybe CAccountMeta)
getWalletMeta cAddr = preview (wsWalletInfos . ix cAddr . wiMeta)

getWSetMetas :: Query [CWalletMeta]
getWSetMetas = toList . fmap _wsiMeta <$> view wsWSetInfos

getWSetMeta :: CId WS -> Query (Maybe CWalletMeta)
getWSetMeta cAddr = preview (wsWSetInfos . ix cAddr . wsiMeta)

getWSetPassLU :: CId WS -> Query (Maybe PassPhraseLU)
getWSetPassLU cAddr = preview (wsWSetInfos . ix cAddr . wsiPassphraseLU)

getWSetSyncTip :: CId WS -> Query (Maybe HeaderHash)
getWSetSyncTip cAddr = preview (wsWSetInfos . ix cAddr . wsiSyncTip)


getWSetAddresses :: Query [CId WS]
getWSetAddresses = HM.keys <$> view wsWSetInfos

getWalletAccounts :: AccountLookupMode
                  -> AccountId
                  -> Query (Maybe [CWAddressMeta])
getWalletAccounts mode wAddr = do
    let fetch which = toList <<$>> preview (wsWalletInfos . ix wAddr . which)
    withAccLookupMode mode (fetch wiAccounts) (fetch wiRemovedAccounts)

doesAccountExist :: AccountLookupMode -> CWAddressMeta -> Query Bool
doesAccountExist mode accAddr@(walletAddrMetaToAccount -> wAddr) = do
    let exists :: Lens' WalletInfo CAddresss -> Query Any
        exists which =
            Any . isJust <$>
            preview (wsWalletInfos . ix wAddr . which . ix accAddr)
    getAny <$>
        withAccLookupMode mode (exists wiAccounts) (exists wiRemovedAccounts)

getTxMeta :: AccountId -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cAddr ctxId = preview $ wsWalletInfos . ix cAddr . wiTxHistory . ix ctxId

getWalletHistory :: AccountId -> Query (Maybe [CTxMeta])
getWalletHistory cAddr = toList <<$>> preview (wsWalletInfos . ix cAddr . wiTxHistory)

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: AccountId -> Query (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache cAddr = view $ wsHistoryCache . at cAddr

createWallet :: AccountId -> CAccountMeta -> Update ()
createWallet cAddr wMeta = wsWalletInfos . at cAddr ?= WalletInfo wMeta mempty mempty mempty

createWSet :: CId WS -> CWalletMeta -> PassPhraseLU -> Update ()
createWSet cAddr wSMeta passLU = wsWSetInfos . at cAddr ?= WalletSetInfo wSMeta passLU genesisHash

addAccount :: CWAddressMeta -> Update ()
addAccount accAddr@CWAddressMeta{..} = do
    wsWalletInfos . ix (walletAddrMetaToAccount accAddr) . wiAccounts . at accAddr ?= ()

-- see also 'removeAccount'
addRemovedAccount :: CWAddressMeta -> Update ()
addRemovedAccount accAddr@CWAddressMeta{..} = do
    let wAddr = walletAddrMetaToAccount accAddr
    wsWalletInfos . ix wAddr . wiAccounts . at accAddr .= Nothing
    wsWalletInfos . ix wAddr . wiRemovedAccounts . at accAddr ?= ()

setWalletMeta :: AccountId -> CAccountMeta -> Update ()
setWalletMeta cAddr wMeta = wsWalletInfos . ix cAddr . wiMeta .= wMeta

setWSetMeta :: CId WS -> CWalletMeta -> Update ()
setWSetMeta cAddr wSMeta = wsWSetInfos . ix cAddr . wsiMeta .= wSMeta

setWSetPassLU :: CId WS -> PassPhraseLU -> Update ()
setWSetPassLU cAddr passLU = wsWSetInfos . ix cAddr . wsiPassphraseLU .= passLU

setWSetSyncTip :: CId WS -> HeaderHash -> Update ()
setWSetSyncTip cAddr hh = wsWSetInfos . ix cAddr . wsiSyncTip .= hh

addWalletHistoryTx :: AccountId -> CTxId -> CTxMeta -> Update ()
addWalletHistoryTx cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId ?= ctxMeta

setWalletHistory :: AccountId -> [(CTxId, CTxMeta)] -> Update ()
setWalletHistory cAddr ctxs = mapM_ (uncurry $ addWalletHistoryTx cAddr) ctxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: AccountId -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId %= Just . fromMaybe ctxMeta

-- NOTE: sets transaction meta only for transactions ids that are already seen
setWalletTransactionMeta :: AccountId -> CTxId -> CTxMeta -> Update ()
setWalletTransactionMeta cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId %= ($> ctxMeta)

removeWSet :: CId WS -> Update ()
removeWSet cAddr = wsWSetInfos . at cAddr .= Nothing

removeWallet :: AccountId -> Update ()
removeWallet cAddr = wsWalletInfos . at cAddr .= Nothing

-- see also 'addRemovedAccount'
removeAccount :: CWAddressMeta -> Update ()
removeAccount accAddr@(walletAddrMetaToAccount -> wAddr) = do
    existed <- wsWalletInfos . ix wAddr . wiAccounts . at accAddr <<.= Nothing
    whenJust existed $ \_ ->
        wsWalletInfos . ix wAddr . wiRemovedAccounts . at accAddr ?= ()

totallyRemoveAccount :: CWAddressMeta -> Update ()
totallyRemoveAccount accAddr@(walletAddrMetaToAccount -> wAddr) = do
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
deriveSafeCopySimple 0 'base ''WS
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
