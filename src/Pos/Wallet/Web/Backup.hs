{-# LANGUAGE TemplateHaskell #-}

module Pos.Wallet.Web.Backup
       ( WalletMetaBackup (..)
       , AccountMetaBackup (..)
       , WalletBackup (..)
       , StateBackup (..)
       ) where

import           Universum

import           Control.Lens               (makeLenses)
-- import           Data.Versions              (SemVer)

import           Pos.Crypto                 (EncryptedSecretKey)
import           Pos.Wallet.Web.ClientTypes (CAccountMeta (..), CWalletMeta (..))

newtype WalletMetaBackup = WalletMetaBackup CWalletMeta
newtype AccountMetaBackup = AccountMetaBackup CAccountMeta

data WalletBackup = WalletBackup
    { _wbSecretKey :: !EncryptedSecretKey
    , _wbMeta      :: !WalletMetaBackup
    , _wbAccounts  :: (HashMap Int AccountMetaBackup)
    }

makeLenses ''WalletBackup

data StateBackup = FullStateBackup [WalletBackup]
