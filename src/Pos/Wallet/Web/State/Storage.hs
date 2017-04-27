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
       , setWalletHistory
       , getWalletHistory
       , addOnlyNewTxMeta
       , setWalletTransactionMeta
       , removeWSet
       , removeWallet
       , removeAccount
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
import           Pos.Txp                    (Utxo)
import           Pos.Types                  (HeaderHash)
import           Pos.Util.BackupPhrase      (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes (Acc, CAccountAddress, CAccountAddress (..),
                                             CAddress, CCoin, CCurrency, CHash, CProfile,
                                             CTxId, CTxMeta, CUpdateInfo, CWalletAddress,
                                             CWalletAssurance, CWalletMeta,
                                             CWalletSetMeta, CWalletType, WS,
                                             walletAddrByAccount)

type TransactionHistory = HashMap CTxId CTxMeta

type CAccounts = HashSet CAccountAddress

data WalletInfo = WalletInfo
  { _wiMeta            :: CWalletMeta
  , _wiAccounts        :: CAccounts
  , _wiRemovedAccounts :: CAccounts
  , _wiTxHistory       :: TransactionHistory
  }

makeLenses ''WalletInfo

data WalletStorage = WalletStorage
    { _wsWSetMetas    :: !(HashMap (CAddress WS) CWalletSetMeta)
    , _wsWalletInfo   :: !(HashMap CWalletAddress WalletInfo)
    , _wsProfile      :: !(Maybe CProfile)
    , _wsReadyUpdates :: [CUpdateInfo]
    , _wsHistoryCache :: !(HashMap CWalletAddress (HeaderHash, Utxo, [TxHistoryEntry]))
    }

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        { _wsWSetMetas = mempty
        , _wsWalletInfo = mempty
        , _wsProfile = mzero
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

getProfile :: Query (Maybe CProfile)
getProfile = view wsProfile

setProfile :: CProfile -> Update ()
setProfile profile = wsProfile ?= profile

getWalletAddresses :: Query [CWalletAddress]
getWalletAddresses = HM.keys <$> view wsWalletInfo

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = map (view wiMeta) . toList <$> view wsWalletInfo

getWalletMeta :: CWalletAddress -> Query (Maybe CWalletMeta)
getWalletMeta cAddr = preview (wsWalletInfo . ix cAddr . wiMeta)

getWSetMetas :: Query [CWalletSetMeta]
getWSetMetas = toList <$> view wsWSetMetas

getWSetMeta :: CAddress WS -> Query (Maybe CWalletSetMeta)
getWSetMeta cAddr = preview (wsWSetMetas . ix cAddr)

getWSetAddresses :: Query [CAddress WS]
getWSetAddresses = HM.keys <$> view wsWSetMetas

getWalletAccounts :: AccountLookupMode
                  -> CWalletAddress
                  -> Query (Maybe [CAccountAddress])
getWalletAccounts mode wAddr = do
    let fetch which = toList <<$>> preview (wsWalletInfo . ix wAddr . which)
    withAccLookupMode mode (fetch wiAccounts) (fetch wiRemovedAccounts)

doesAccountExist :: AccountLookupMode -> CAccountAddress -> Query Bool
doesAccountExist mode accAddr@(walletAddrByAccount -> wAddr) = do
    let exists :: Lens' WalletInfo CAccounts -> Query Any
        exists which =
            Any . isJust <$>
            preview (wsWalletInfo . ix wAddr . which . ix accAddr)
    getAny <$>
        withAccLookupMode mode (exists wiAccounts) (exists wiRemovedAccounts)

getTxMeta :: CWalletAddress -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cAddr ctxId = preview $ wsWalletInfo . ix cAddr . wiTxHistory . ix ctxId

getWalletHistory :: CWalletAddress -> Query (Maybe [CTxMeta])
getWalletHistory cAddr = toList <<$>> preview (wsWalletInfo . ix cAddr . wiTxHistory)

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: CWalletAddress -> Query (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache cAddr = view $ wsHistoryCache . at cAddr

createWallet :: CWalletAddress -> CWalletMeta -> Update ()
createWallet cAddr wMeta = wsWalletInfo . at cAddr ?= WalletInfo wMeta mempty mempty mempty

createWSet :: CAddress WS -> CWalletSetMeta -> Update ()
createWSet cAddr wSMeta = wsWSetMetas . at cAddr ?= wSMeta

addAccount :: CAccountAddress -> Update ()
addAccount accAddr@CAccountAddress{..} = do
    wsWalletInfo . ix (walletAddrByAccount accAddr) . wiAccounts . at accAddr ?= ()

-- see also 'removeAccount'
addRemovedAccount :: CAccountAddress -> Update ()
addRemovedAccount accAddr@CAccountAddress{..} = do
    let wAddr = walletAddrByAccount accAddr
    wsWalletInfo . ix wAddr . wiAccounts . at accAddr .= Nothing
    wsWalletInfo . ix wAddr . wiRemovedAccounts . at accAddr ?= ()

setWalletMeta :: CWalletAddress -> CWalletMeta -> Update ()
setWalletMeta cAddr wMeta = wsWalletInfo . ix cAddr . wiMeta .= wMeta

setWSetMeta :: CAddress WS -> CWalletSetMeta -> Update ()
setWSetMeta cAddr wSMeta = wsWSetMetas . at cAddr ?= wSMeta

addWalletHistoryTx :: CWalletAddress -> CTxId -> CTxMeta -> Update ()
addWalletHistoryTx cAddr ctxId ctxMeta =
    wsWalletInfo . ix cAddr . wiTxHistory . at ctxId ?= ctxMeta

setWalletHistory :: CWalletAddress -> [(CTxId, CTxMeta)] -> Update ()
setWalletHistory cAddr ctxs = mapM_ (uncurry $ addWalletHistoryTx cAddr) ctxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: CWalletAddress -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cAddr ctxId ctxMeta =
    wsWalletInfo . ix cAddr . wiTxHistory . at ctxId %= Just . maybe ctxMeta identity

-- NOTE: sets transaction meta only for transactions ids that are already seen
setWalletTransactionMeta :: CWalletAddress -> CTxId -> CTxMeta -> Update ()
setWalletTransactionMeta cAddr ctxId ctxMeta =
    wsWalletInfo . ix cAddr . wiTxHistory . at ctxId %= ($> ctxMeta)

removeWSet :: CAddress WS -> Update ()
removeWSet cAddr = wsWSetMetas . at cAddr .= Nothing

removeWallet :: CWalletAddress -> Update ()
removeWallet cAddr = wsWalletInfo . at cAddr .= Nothing

-- see also 'addRemovedAccount'
removeAccount :: CAccountAddress -> Update ()
removeAccount accAddr@(walletAddrByAccount -> wAddr) = do
    existed <- wsWalletInfo . ix wAddr . wiAccounts . at accAddr <<.= Nothing
    whenJust existed $ \_ ->
        wsWalletInfo . ix wAddr . wiRemovedAccounts . at accAddr ?= ()

addUpdate :: CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= drop 1

testReset :: Update ()
testReset = put def

updateHistoryCache :: CWalletAddress -> HeaderHash -> Utxo -> [TxHistoryEntry] -> Update ()
updateHistoryCache cAddr cHash utxo cTxs =
    wsHistoryCache . at cAddr ?= (cHash, utxo, cTxs)

deriveSafeCopySimple 0 'base ''CCoin
deriveSafeCopySimple 0 'base ''CProfile
deriveSafeCopySimple 0 'base ''CHash
deriveSafeCopySimple 0 'base ''CAddress
deriveSafeCopySimple 0 'base ''WS
deriveSafeCopySimple 0 'base ''Acc
deriveSafeCopySimple 0 'base ''BackupPhrase
deriveSafeCopySimple 0 'base ''CWalletAddress
deriveSafeCopySimple 0 'base ''CAccountAddress
deriveSafeCopySimple 0 'base ''CCurrency
deriveSafeCopySimple 0 'base ''CWalletType
deriveSafeCopySimple 0 'base ''CWalletAssurance
deriveSafeCopySimple 0 'base ''CWalletMeta
deriveSafeCopySimple 0 'base ''CWalletSetMeta
deriveSafeCopySimple 0 'base ''CTxId
deriveSafeCopySimple 0 'base ''TxHistoryEntry
deriveSafeCopySimple 0 'base ''CTxMeta
deriveSafeCopySimple 0 'base ''CUpdateInfo
deriveSafeCopySimple 0 'base ''AccountLookupMode
deriveSafeCopySimple 0 'base ''WalletInfo
deriveSafeCopySimple 0 'base ''WalletStorage
