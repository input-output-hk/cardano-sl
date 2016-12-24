{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/Storage.hs
module Pos.Wallet.Web.State.Storage
       (
         WalletStorage (..)
       , Query
       , Update
       , getWalletMetas
       , getWalletMeta
       , createWallet
       , setWalletMeta
       , setWalletHistory
       , getWalletHistory
       , addOnlyNewHistory
       , removeWallet
       ) where

import           Control.Lens               (at, ix, makeClassy, preview, view, (%=),
                                             (.=), _1, _2, _Just)
import           Data.Default               (Default, def)
import           Data.HashMap.Strict        (elems)
import           Data.List                  (unionBy)
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CHash, CTType, CTx,
                                             CTxId, CTxMeta, CWalletMeta, CWalletType,
                                             ctId)
import           Universum

data WalletStorage = WalletStorage
    {
      -- TODO: implement [CTx] as a Vector
      _wsWalletMetas :: !(HashMap CAddress (CWalletMeta, [CTx]))
    }

makeClassy ''WalletStorage

instance Default WalletStorage where
    def =
        WalletStorage
        {
          _wsWalletMetas = mempty
        }

type Query a = forall m. (MonadReader WalletStorage m) => m a
type Update a = forall m. ({-MonadThrow m, -}MonadState WalletStorage m) => m a

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = elems . map fst <$> view wsWalletMetas

getWalletMeta :: CAddress -> Query (Maybe CWalletMeta)
getWalletMeta cAddr = preview (wsWalletMetas . ix cAddr . _1)

getWalletHistory :: CAddress -> Query (Maybe [CTx])
getWalletHistory cAddr = preview (wsWalletMetas . ix cAddr . _2)

createWallet :: CAddress -> CWalletMeta -> Update ()
createWallet cAddr wMeta = wsWalletMetas . at cAddr .= Just (wMeta, mempty)

setWalletMeta :: CAddress -> CWalletMeta -> Update ()
setWalletMeta cAddr wMeta = wsWalletMetas . at cAddr . _Just . _1 .= wMeta

setWalletHistory :: CAddress -> [CTx] -> Update ()
setWalletHistory cAddr wHistory = wsWalletMetas . at cAddr . _Just . _2 .= wHistory

-- FIXME: this will be removed later (temporary solution)
addOnlyNewHistory :: CAddress -> [CTx] -> Update ()
addOnlyNewHistory cAddr wHistory = wsWalletMetas . at cAddr . _Just . _2 %= unionBy ((==) `on` ctId) wHistory

removeWallet :: CAddress -> Update ()
removeWallet cAddr = wsWalletMetas . at cAddr .= Nothing

deriveSafeCopySimple 0 'base ''CHash
deriveSafeCopySimple 0 'base ''CAddress
deriveSafeCopySimple 0 'base ''CCurrency
deriveSafeCopySimple 0 'base ''CWalletType
deriveSafeCopySimple 0 'base ''CWalletMeta
deriveSafeCopySimple 0 'base ''CTxId
deriveSafeCopySimple 0 'base ''CTType
deriveSafeCopySimple 0 'base ''CTxMeta
deriveSafeCopySimple 0 'base ''CTx
deriveSafeCopySimple 0 'base ''WalletStorage
