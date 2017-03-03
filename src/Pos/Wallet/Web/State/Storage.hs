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
       , getWalletMetas
       , getWalletMeta
       , getTxMeta
       , getUpdates
       , getNextUpdate
       , createWallet
       , setWalletMeta
       , setWalletHistory
       , getWalletHistory
       , addOnlyNewTxMeta
       , setWalletTransactionMeta
       , removeWallet
       , addUpdate
       , removeNextUpdate
       , testReset
       ) where

import           Control.Lens               (at, ix, makeClassy, (%=), (.=), _Just, _head)
import           Control.Monad.State.Class  (put)
import           Data.Default               (Default, def)
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CHash, CProfile, CTxId,
                                             CTxMeta, CUpdateInfo, CWalletMeta,
                                             CWalletType)
import           Universum

type TransactionHistory = HashMap CTxId CTxMeta

data WalletStorage = WalletStorage
    {
      _wsWalletMetas  :: !(HashMap CAddress (CWalletMeta, TransactionHistory))
    , _wsProfile      :: !(Maybe CProfile)
    , _wsReadyUpdates :: [CUpdateInfo]
    }

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        {
          _wsWalletMetas = mempty
        , _wsProfile = mzero
        , _wsReadyUpdates = mempty
        }

type Query a = forall m. (MonadReader WalletStorage m) => m a
type Update a = forall m. ({-MonadThrow m, -}MonadState WalletStorage m) => m a

getProfile :: Query (Maybe CProfile)
getProfile = view wsProfile

setProfile :: CProfile -> Update ()
setProfile profile = wsProfile .= Just profile

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = toList . map fst <$> view wsWalletMetas

getWalletMeta :: CAddress -> Query (Maybe CWalletMeta)
getWalletMeta cAddr = preview (wsWalletMetas . ix cAddr . _1)

getTxMeta :: CAddress -> CTxId -> Query (Maybe CTxMeta)
getTxMeta cAddr ctxId = preview $ wsWalletMetas . at cAddr . _Just . _2 . at ctxId . _Just

getWalletHistory :: CAddress -> Query (Maybe [CTxMeta])
getWalletHistory cAddr = fmap toList <$> preview (wsWalletMetas . ix cAddr . _2)

getUpdates :: Query [CUpdateInfo]
getUpdates = view wsReadyUpdates

getNextUpdate :: Query (Maybe CUpdateInfo)
getNextUpdate = preview (wsReadyUpdates . _head)

createWallet :: CAddress -> CWalletMeta -> Update ()
createWallet cAddr wMeta = wsWalletMetas . at cAddr .= Just (wMeta, mempty)

setWalletMeta :: CAddress -> CWalletMeta -> Update ()
setWalletMeta cAddr wMeta = wsWalletMetas . at cAddr . _Just . _1 .= wMeta

addWalletHistoryTx :: CAddress -> CTxId -> CTxMeta -> Update ()
addWalletHistoryTx cAddr ctxId ctxMeta = wsWalletMetas . at cAddr . _Just . _2 . at ctxId .= Just ctxMeta

setWalletHistory :: CAddress -> [(CTxId, CTxMeta)] -> Update ()
setWalletHistory cAddr ctxs = () <$ mapM (uncurry $ addWalletHistoryTx cAddr) ctxs

-- FIXME: this will be removed later (temporary solution)
addOnlyNewTxMeta :: CAddress -> CTxId -> CTxMeta -> Update ()
addOnlyNewTxMeta cAddr ctxId ctxMeta = wsWalletMetas . at cAddr . _Just . _2 . at ctxId %= Just . maybe ctxMeta identity

-- NOTE: sets transaction meta only for transactions ids that are already seen
setWalletTransactionMeta :: CAddress -> CTxId -> CTxMeta -> Update ()
setWalletTransactionMeta cAddr ctxId ctxMeta = wsWalletMetas . at cAddr . _Just . _2 . at ctxId %= fmap (const ctxMeta)

removeWallet :: CAddress -> Update ()
removeWallet cAddr = wsWalletMetas . at cAddr .= Nothing

addUpdate :: CUpdateInfo -> Update ()
addUpdate ui = wsReadyUpdates %= (++ [ui])

removeNextUpdate :: Update ()
removeNextUpdate = wsReadyUpdates %= \case
    [] -> []
    (_:as) -> as

testReset :: Update ()
testReset = put def

deriveSafeCopySimple 0 'base ''CProfile
deriveSafeCopySimple 0 'base ''CHash
deriveSafeCopySimple 0 'base ''CAddress
deriveSafeCopySimple 0 'base ''CCurrency
deriveSafeCopySimple 0 'base ''CWalletType
deriveSafeCopySimple 0 'base ''CWalletMeta
deriveSafeCopySimple 0 'base ''CTxId
deriveSafeCopySimple 0 'base ''CTxMeta
deriveSafeCopySimple 0 'base ''CUpdateInfo
deriveSafeCopySimple 0 'base ''WalletStorage
