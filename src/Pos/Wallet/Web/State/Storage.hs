{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/Storage.hs
module Pos.Wallet.Web.State.Storage
       (
         WalletStorage (..)
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
       , setWalletMeta
       , setWSetMeta
       , setAccountHistory
       , getAccountHistory
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

import           Control.Lens               (at, ix, makeClassy, (%=), (.=), (?=), _Just,
                                             _head)
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

data WalletStorage = WalletStorage
    { _wsWSetMetas    :: !(HashMap (CAddress WS) CWalletSetMeta)
    , _wsWalletMetas  :: !(HashMap CWalletAddress (CWalletMeta, CAccounts))
    , _wsAccountTxs   :: !(HashMap (CAddress Acc) TransactionHistory)
    , _wsProfile      :: !(Maybe CProfile)
    , _wsReadyUpdates :: [CUpdateInfo]
    , _wsHistoryCache :: !(HashMap CWalletAddress (HeaderHash, Utxo, [TxHistoryEntry]))
    }

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        { _wsWSetMetas = mempty
        , _wsWalletMetas = mempty
        , _wsAccountTxs = mempty
        , _wsProfile = mzero
        , _wsReadyUpdates = mempty
        , _wsHistoryCache = mempty
        }

type Query a = forall m. (MonadReader WalletStorage m) => m a
type Update a = forall m. ({-MonadThrow m, -}MonadState WalletStorage m) => m a

getProfile :: Query (Maybe CProfile)
getProfile = view wsProfile

setProfile :: CProfile -> Update ()
setProfile profile = wsProfile ?= profile

getWalletAddresses :: Query [CWalletAddress]
getWalletAddresses = HM.keys <$> view wsWalletMetas

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = map fst . toList <$> view wsWalletMetas

getWalletMeta :: CWalletAddress -> Query (Maybe CWalletMeta)
getWalletMeta cAddr = preview (wsWalletMetas . ix cAddr . _1)

getWSetMetas :: Query [CWalletSetMeta]
getWSetMetas = toList <$> view wsWSetMetas

getWSetMeta :: CAddress WS -> Query (Maybe CWalletSetMeta)
getWSetMeta cAddr = preview (wsWSetMetas . ix cAddr)

getWSetAddresses :: Query [CAddress WS]
getWSetAddresses = HM.keys <$> view wsWSetMetas

getWalletAccounts :: CWalletAddress -> Query (Maybe [CAccountAddress])
getWalletAccounts wAddr = toList <<$>> preview (wsWalletMetas . ix wAddr . _2)

doesAccountExist :: CAddress Acc -> Query Bool
doesAccountExist accAddr = isJust <$> view (wsAccountTxs . at accAddr)

getTxMeta :: CAddress Acc -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cAddr ctxId = preview $ wsAccountTxs . at cAddr . _Just . at ctxId . _Just

getAccountHistory :: CAddress Acc -> Query (Maybe [CTxMeta])
getAccountHistory cAddr = fmap toList <$> view (wsAccountTxs . at cAddr)

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

getHistoryCache :: CWalletAddress -> Query (Maybe (HeaderHash, Utxo, [TxHistoryEntry]))
getHistoryCache cAddr = view $ wsHistoryCache . at cAddr

createWallet :: CWalletAddress -> CWalletMeta -> Update ()
createWallet cAddr wMeta = wsWalletMetas . at cAddr ?= (wMeta, mempty)

createWSet :: CAddress WS -> CWalletSetMeta -> Update ()
createWSet cAddr wSMeta = wsWSetMetas . at cAddr ?= wSMeta

addAccount :: CAccountAddress -> Update ()
addAccount accAddr@CAccountAddress{..} = do
    wsWalletMetas . at (walletAddrByAccount accAddr) . _Just . _2 . at accAddr ?= ()
    wsAccountTxs . at caaAddress ?= mempty

setWalletMeta :: CWalletAddress -> CWalletMeta -> Update ()
setWalletMeta cAddr wMeta = wsWalletMetas . at cAddr . _Just . _1 .= wMeta

setWSetMeta :: CAddress WS -> CWalletSetMeta -> Update ()
setWSetMeta cAddr wSMeta = wsWSetMetas . at cAddr ?= wSMeta

addAccountHistoryTx :: CAddress Acc -> CTxId -> CTxMeta -> Update ()
addAccountHistoryTx cAddr ctxId ctxMeta =
    wsAccountTxs . at cAddr . _Just . at ctxId ?= ctxMeta

setAccountHistory :: CAddress Acc -> [(CTxId, CTxMeta)] -> Update ()
setAccountHistory cAddr ctxs = () <$ mapM (uncurry $ addAccountHistoryTx cAddr) ctxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: CAddress Acc -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cAddr ctxId ctxMeta =
    wsAccountTxs . at cAddr . _Just . at ctxId %= Just . maybe ctxMeta identity

-- NOTE: sets transaction meta only for transactions ids that are already seen
setWalletTransactionMeta :: CAddress Acc -> CTxId -> CTxMeta -> Update ()
setWalletTransactionMeta cAddr ctxId ctxMeta =
    wsAccountTxs . at cAddr . _Just . at ctxId %= fmap (const ctxMeta)

removeWSet :: CAddress WS -> Update ()
removeWSet cAddr = wsWSetMetas . at cAddr .= Nothing

removeWallet :: CWalletAddress -> Update ()
removeWallet cAddr = wsWalletMetas . at cAddr .= Nothing

removeAccount :: CAccountAddress -> Update ()
removeAccount accAddr@CAccountAddress {..} = do
    wsWalletMetas . at (walletAddrByAccount accAddr) . _Just . _2 . at accAddr .=
        Nothing
    wsAccountTxs . at caaAddress .= Nothing

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
deriveSafeCopySimple 0 'base ''WalletStorage
