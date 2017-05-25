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
       , getWalletAddresses
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
import           Pos.Wallet.Web.ClientTypes (Acc, CAccountAddress, CAccountAddress (..),
                                             CAddress, CCoin, CHash, CProfile, CTxId,
                                             CTxMeta, CUpdateInfo, CWalletMeta,
                                             CWalletSetAssurance, CWalletSetMeta,
                                             PassPhraseLU, WS, WalletAddress,
                                             walletAddrByAccount)

type TransactionHistory = HashMap CTxId CTxMeta

type CAccounts = HashSet CAccountAddress

data WalletSetInfo = WalletSetInfo
    { _wsiMeta         :: CWalletSetMeta
    , _wsiPassphraseLU :: PassPhraseLU
    , _wsiSyncTip      :: HeaderHash
    }

makeLenses ''WalletSetInfo

data WalletInfo = WalletInfo
    { _wiMeta            :: CWalletMeta
    , _wiAccounts        :: CAccounts
    , _wiRemovedAccounts :: CAccounts
    , _wiTxHistory       :: TransactionHistory
    }

makeLenses ''WalletInfo

data WalletStorage = WalletStorage
    { _wsWSetInfos    :: !(HashMap (CAddress WS) WalletSetInfo)
    , _wsWalletInfos  :: !(HashMap WalletAddress WalletInfo)
    , _wsProfile      :: !CProfile
    , _wsReadyUpdates :: [CUpdateInfo]
    , _wsHistoryCache :: !(HashMap WalletAddress (HeaderHash, Utxo, [TxHistoryEntry]))
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

getWalletAddresses :: Query [WalletAddress]
getWalletAddresses = HM.keys <$> view wsWalletInfos

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = map (view wiMeta) . toList <$> view wsWalletInfos

getWalletMeta :: WalletAddress -> Query (Maybe CWalletMeta)
getWalletMeta cAddr = preview (wsWalletInfos . ix cAddr . wiMeta)

getWSetMetas :: Query [CWalletSetMeta]
getWSetMetas = toList . fmap _wsiMeta <$> view wsWSetInfos

getWSetMeta :: CAddress WS -> Query (Maybe CWalletSetMeta)
getWSetMeta cAddr = preview (wsWSetInfos . ix cAddr . wsiMeta)

getWSetPassLU :: CAddress WS -> Query (Maybe PassPhraseLU)
getWSetPassLU cAddr = preview (wsWSetInfos . ix cAddr . wsiPassphraseLU)

getWSetSyncTip :: CAddress WS -> Query (Maybe HeaderHash)
getWSetSyncTip cAddr = preview (wsWSetInfos . ix cAddr . wsiSyncTip)


getWSetAddresses :: Query [CAddress WS]
getWSetAddresses = HM.keys <$> view wsWSetInfos

getWalletAccounts :: AccountLookupMode
                  -> WalletAddress
                  -> Query (Maybe [CAccountAddress])
getWalletAccounts mode wAddr = do
    let fetch which = toList <<$>> preview (wsWalletInfos . ix wAddr . which)
    withAccLookupMode mode (fetch wiAccounts) (fetch wiRemovedAccounts)

doesAccountExist :: AccountLookupMode -> CAccountAddress -> Query Bool
doesAccountExist mode accAddr@(walletAddrByAccount -> wAddr) = do
    let exists :: Lens' WalletInfo CAccounts -> Query Any
        exists which =
            Any . isJust <$>
            preview (wsWalletInfos . ix wAddr . which . ix accAddr)
    getAny <$>
        withAccLookupMode mode (exists wiAccounts) (exists wiRemovedAccounts)

getTxMeta :: WalletAddress -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cAddr ctxId = preview $ wsWalletInfos . ix cAddr . wiTxHistory . ix ctxId

getWalletHistory :: WalletAddress -> Query (Maybe [CTxMeta])
getWalletHistory cAddr = toList <<$>> preview (wsWalletInfos . ix cAddr . wiTxHistory)

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: WalletAddress -> Query (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache cAddr = view $ wsHistoryCache . at cAddr

createWallet :: WalletAddress -> CWalletMeta -> Update ()
createWallet cAddr wMeta = wsWalletInfos . at cAddr ?= WalletInfo wMeta mempty mempty mempty

createWSet :: CAddress WS -> CWalletSetMeta -> PassPhraseLU -> Update ()
createWSet cAddr wSMeta passLU = wsWSetInfos . at cAddr ?= WalletSetInfo wSMeta passLU genesisHash

addAccount :: CAccountAddress -> Update ()
addAccount accAddr@CAccountAddress{..} = do
    wsWalletInfos . ix (walletAddrByAccount accAddr) . wiAccounts . at accAddr ?= ()

-- see also 'removeAccount'
addRemovedAccount :: CAccountAddress -> Update ()
addRemovedAccount accAddr@CAccountAddress{..} = do
    let wAddr = walletAddrByAccount accAddr
    wsWalletInfos . ix wAddr . wiAccounts . at accAddr .= Nothing
    wsWalletInfos . ix wAddr . wiRemovedAccounts . at accAddr ?= ()

setWalletMeta :: WalletAddress -> CWalletMeta -> Update ()
setWalletMeta cAddr wMeta = wsWalletInfos . ix cAddr . wiMeta .= wMeta

setWSetMeta :: CAddress WS -> CWalletSetMeta -> Update ()
setWSetMeta cAddr wSMeta = wsWSetInfos . ix cAddr . wsiMeta .= wSMeta

setWSetPassLU :: CAddress WS -> PassPhraseLU -> Update ()
setWSetPassLU cAddr passLU = wsWSetInfos . ix cAddr . wsiPassphraseLU .= passLU

setWSetSyncTip :: CAddress WS -> HeaderHash -> Update ()
setWSetSyncTip cAddr hh = wsWSetInfos . ix cAddr . wsiSyncTip .= hh

addWalletHistoryTx :: WalletAddress -> CTxId -> CTxMeta -> Update ()
addWalletHistoryTx cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId ?= ctxMeta

setWalletHistory :: WalletAddress -> [(CTxId, CTxMeta)] -> Update ()
setWalletHistory cAddr ctxs = mapM_ (uncurry $ addWalletHistoryTx cAddr) ctxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: WalletAddress -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId %= Just . fromMaybe ctxMeta

-- NOTE: sets transaction meta only for transactions ids that are already seen
setWalletTransactionMeta :: WalletAddress -> CTxId -> CTxMeta -> Update ()
setWalletTransactionMeta cAddr ctxId ctxMeta =
    wsWalletInfos . ix cAddr . wiTxHistory . at ctxId %= ($> ctxMeta)

removeWSet :: CAddress WS -> Update ()
removeWSet cAddr = wsWSetInfos . at cAddr .= Nothing

removeWallet :: WalletAddress -> Update ()
removeWallet cAddr = wsWalletInfos . at cAddr .= Nothing

-- see also 'addRemovedAccount'
removeAccount :: CAccountAddress -> Update ()
removeAccount accAddr@(walletAddrByAccount -> wAddr) = do
    existed <- wsWalletInfos . ix wAddr . wiAccounts . at accAddr <<.= Nothing
    whenJust existed $ \_ ->
        wsWalletInfos . ix wAddr . wiRemovedAccounts . at accAddr ?= ()

totallyRemoveAccount :: CAccountAddress -> Update ()
totallyRemoveAccount accAddr@(walletAddrByAccount -> wAddr) = do
    wsWalletInfos . ix wAddr . wiAccounts . at accAddr .= Nothing
    wsWalletInfos . ix wAddr . wiRemovedAccounts . at accAddr .= Nothing

addUpdate :: CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= drop 1

testReset :: Update ()
testReset = put def

updateHistoryCache :: WalletAddress -> HeaderHash -> Utxo -> [TxHistoryEntry] -> Update ()
updateHistoryCache cAddr cHash utxo cTxs =
    wsHistoryCache . at cAddr ?= (cHash, utxo, cTxs)

deriveSafeCopySimple 0 'base ''CCoin
deriveSafeCopySimple 0 'base ''CProfile
deriveSafeCopySimple 0 'base ''CHash
deriveSafeCopySimple 0 'base ''CAddress
deriveSafeCopySimple 0 'base ''WS
deriveSafeCopySimple 0 'base ''Acc
deriveSafeCopySimple 0 'base ''BackupPhrase
deriveSafeCopySimple 0 'base ''WalletAddress
deriveSafeCopySimple 0 'base ''CAccountAddress
deriveSafeCopySimple 0 'base ''CWalletSetAssurance
deriveSafeCopySimple 0 'base ''CWalletMeta
deriveSafeCopySimple 0 'base ''CWalletSetMeta
deriveSafeCopySimple 0 'base ''CTxId
deriveSafeCopySimple 0 'base ''TxHistoryEntry
deriveSafeCopySimple 0 'base ''CTxMeta
deriveSafeCopySimple 0 'base ''CUpdateInfo
deriveSafeCopySimple 0 'base ''AccountLookupMode
deriveSafeCopySimple 0 'base ''WalletInfo
deriveSafeCopySimple 0 'base ''WalletSetInfo
deriveSafeCopySimple 0 'base ''WalletStorage
