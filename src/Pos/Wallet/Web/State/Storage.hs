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
import           Pos.Wallet.Web.ClientTypes (AccountId, Addr, CAccountMeta,
                                             CCoin, CHash, CId, CProfile, CTxId, CTxMeta,
                                             CUpdateInfo, CWAddressMeta (..),
                                             CWalletAssurance, CWalletMeta, PassPhraseLU,
                                             Wal, walletAddrMetaToAccount)

type TransactionHistory = HashMap CTxId CTxMeta

type CAddresses = HashSet CWAddressMeta

data WalletSetInfo = WalletSetInfo
    { _wsiMeta         :: CWalletMeta
    , _wsiPassphraseLU :: PassPhraseLU
    , _wsiSyncTip      :: HeaderHash
    }

makeLenses ''WalletSetInfo

data WalletInfo = WalletInfo
    { _wiMeta            :: CAccountMeta
    , _wiAccounts        :: CAddresses
    , _wiRemovedAccounts :: CAddresses
    }

makeLenses ''WalletInfo

data WalletStorage = WalletStorage
    { _wsWSetInfos    :: !(HashMap (CId Wal) WalletSetInfo)
    , _wsWalletInfos  :: !(HashMap AccountId WalletInfo)
    , _wsProfile      :: !CProfile
    , _wsReadyUpdates :: [CUpdateInfo]
    , _wsTxHistory    :: !(HashMap (CId Wal) TransactionHistory)
    , _wsHistoryCache :: !(HashMap (CId Wal) (HeaderHash, Utxo, [TxHistoryEntry]))
    }

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        { _wsWSetInfos    = mempty
        , _wsWalletInfos  = mempty
        , _wsProfile      = def
        , _wsReadyUpdates = mempty
        , _wsTxHistory    = mempty
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
setProfile cProfile = wsProfile .= cProfile

getWAddressIds :: Query [AccountId]
getWAddressIds = HM.keys <$> view wsWalletInfos

getAccountMetas :: Query [CAccountMeta]
getAccountMetas = map (view wiMeta) . toList <$> view wsWalletInfos

getAccountMeta :: AccountId -> Query (Maybe CAccountMeta)
getAccountMeta accId = preview (wsWalletInfos . ix accId . wiMeta)

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = toList . fmap _wsiMeta <$> view wsWSetInfos

getWalletMeta :: CId Wal -> Query (Maybe CWalletMeta)
getWalletMeta cWalId = preview (wsWSetInfos . ix cWalId . wsiMeta)

getWalletPassLU :: CId Wal -> Query (Maybe PassPhraseLU)
getWalletPassLU cWalId = preview (wsWSetInfos . ix cWalId . wsiPassphraseLU)

getWalletSyncTip :: CId Wal -> Query (Maybe HeaderHash)
getWalletSyncTip cWalId = preview (wsWSetInfos . ix cWalId . wsiSyncTip)


getWalletAddresses :: Query [CId Wal]
getWalletAddresses = HM.keys <$> view wsWSetInfos

getAccountWAddresses :: AccountLookupMode
                  -> AccountId
                  -> Query (Maybe [CWAddressMeta])
getAccountWAddresses mode accId = do
    let fetch which = toList <<$>> preview (wsWalletInfos . ix accId . which)
    withAccLookupMode mode (fetch wiAccounts) (fetch wiRemovedAccounts)

doesWAddressExist :: AccountLookupMode -> CWAddressMeta -> Query Bool
doesWAddressExist mode accAddr@(walletAddrMetaToAccount -> wAddr) = do
    let exists :: Lens' WalletInfo CAddresses -> Query Any
        exists which =
            Any . isJust <$>
            preview (wsWalletInfos . ix wAddr . which . ix accAddr)
    getAny <$>
        withAccLookupMode mode (exists wiAccounts) (exists wiRemovedAccounts)

getTxMeta :: CId Wal -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cWalId ctxId = preview $ wsTxHistory . ix cWalId . ix ctxId

getWalletTxHistory :: CId Wal -> Query (Maybe [CTxMeta])
getWalletTxHistory cWalId = toList <<$>> preview (wsTxHistory . ix cWalId)

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: CId Wal -> Query (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache cWalId = view $ wsHistoryCache . at cWalId

createAccount :: AccountId -> CAccountMeta -> Update ()
createAccount accId cAccMeta = wsWalletInfos . at accId ?= WalletInfo cAccMeta mempty mempty

createWallet :: CId Wal -> CWalletMeta -> PassPhraseLU -> Update ()
createWallet cWalId cWalMeta passLU = wsWSetInfos . at cWalId ?= WalletSetInfo cWalMeta passLU genesisHash

addWAddress :: CWAddressMeta -> Update ()
addWAddress addr@CWAddressMeta{..} =
    wsWalletInfos . ix (walletAddrMetaToAccount addr) . wiAccounts . at addr ?= ()

-- see also 'removeWAddress'
addRemovedAccount :: CWAddressMeta -> Update ()
addRemovedAccount addr@CWAddressMeta{..} = do
    let acc = walletAddrMetaToAccount addr
    wsWalletInfos . ix acc . wiAccounts . at addr .= Nothing
    wsWalletInfos . ix acc . wiRemovedAccounts . at addr ?= ()

setAccountMeta :: AccountId -> CAccountMeta -> Update ()
setAccountMeta accId cAccMeta = wsWalletInfos . ix accId . wiMeta .= cAccMeta

setWalletMeta :: CId Wal -> CWalletMeta -> Update ()
setWalletMeta cWalId cWalMeta = wsWSetInfos . ix cWalId . wsiMeta .= cWalMeta

setWalletPassLU :: CId Wal -> PassPhraseLU -> Update ()
setWalletPassLU cWalId passLU = wsWSetInfos . ix cWalId . wsiPassphraseLU .= passLU

setWalletSyncTip :: CId Wal -> HeaderHash -> Update ()
setWalletSyncTip cWalId hh = wsWSetInfos . ix cWalId . wsiSyncTip .= hh

addWalletTxHistory :: CId Wal -> CTxId -> CTxMeta -> Update ()
addWalletTxHistory cWalId cTxId cTxMeta =
    wsTxHistory . ix cWalId . at cTxId ?= cTxMeta

setWalletTxHistory :: CId Wal -> [(CTxId, CTxMeta)] -> Update ()
setWalletTxHistory cWalId cTxs = mapM_ (uncurry $ addWalletTxHistory cWalId) cTxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: CId Wal -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cWalId cTxId cTxMeta =
    wsTxHistory . ix cWalId . at cTxId %= Just . fromMaybe cTxMeta

-- NOTE: sets transaction meta only for transactions ids that are already seen
setWalletTxMeta :: CId Wal -> CTxId -> CTxMeta -> Update ()
setWalletTxMeta cWalId cTxId cTxMeta =
    wsTxHistory . ix cWalId . at cTxId %= ($> cTxMeta)

removeWallet :: CId Wal -> Update ()
removeWallet cWalId = wsWSetInfos . at cWalId .= Nothing

removeAccount :: AccountId -> Update ()
removeAccount accId = wsWalletInfos . at accId .= Nothing

-- see also 'addRemovedAccount'
removeWAddress :: CWAddressMeta -> Update ()
removeWAddress addr@(walletAddrMetaToAccount -> wAddr) = do
    existed <- wsWalletInfos . ix wAddr . wiAccounts . at addr <<.= Nothing
    whenJust existed $ \_ ->
        wsWalletInfos . ix wAddr . wiRemovedAccounts . at addr ?= ()

totallyRemoveWAddress :: CWAddressMeta -> Update ()
totallyRemoveWAddress addr@(walletAddrMetaToAccount -> wAddr) = do
    wsWalletInfos . ix wAddr . wiAccounts . at addr .= Nothing
    wsWalletInfos . ix wAddr . wiRemovedAccounts . at addr .= Nothing

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
deriveSafeCopySimple 0 'base ''TxHistoryEntry
deriveSafeCopySimple 0 'base ''CTxMeta
deriveSafeCopySimple 0 'base ''CUpdateInfo
deriveSafeCopySimple 0 'base ''AccountLookupMode
deriveSafeCopySimple 0 'base ''WalletInfo
deriveSafeCopySimple 0 'base ''WalletSetInfo
deriveSafeCopySimple 0 'base ''WalletStorage
