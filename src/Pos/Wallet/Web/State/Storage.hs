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
       , addWalletMeta
       , removeWallet
       ) where

import           Control.Lens               (at, makeClassy, view, (.=))
import           Data.Default               (Default, def)
import           Data.HashMap.Strict        (elems)
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CHash, CWalletMeta,
                                             CWalletType)
import           Universum

data WalletStorage = WalletStorage
    {
      _wsWalletMetas :: !(HashMap CAddress CWalletMeta)
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

-- getDummyAttribute :: Query Int
-- getDummyAttribute = view dummyAttribute
--
-- setDummyAttribute :: Int -> Update ()
-- setDummyAttribute x = dummyAttribute .= x

getWalletMetas :: Query [CWalletMeta]
getWalletMetas = elems <$> view wsWalletMetas

getWalletMeta :: CAddress -> Query (Maybe CWalletMeta)
getWalletMeta cAddr = view (wsWalletMetas . at cAddr)

addWalletMeta :: CAddress -> CWalletMeta -> Update ()
addWalletMeta cAddr wMeta = wsWalletMetas . at cAddr .= Just wMeta

removeWallet :: CAddress -> Update ()
removeWallet cAddr = wsWalletMetas . at cAddr .= Nothing

deriveSafeCopySimple 0 'base ''CHash
deriveSafeCopySimple 0 'base ''CAddress
deriveSafeCopySimple 0 'base ''CCurrency
deriveSafeCopySimple 0 'base ''CWalletType
deriveSafeCopySimple 0 'base ''CWalletMeta
deriveSafeCopySimple 0 'base ''WalletStorage
