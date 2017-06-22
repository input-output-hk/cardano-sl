{-# LANGUAGE TemplateHaskell #-}

module Pos.Wallet.Web.Backup
       ( WalletMetaBackup (..)
       , AccountMetaBackup (..)
       , WalletBackup (..)
       , StateBackup (..)
       , wbSecretKey
       , wbMeta
       , wbAccounts
       , currentBackupFormatVersion
       ) where

import           Universum

import           Control.Lens               (makeLenses)

import           Pos.Crypto                 (EncryptedSecretKey)
import           Pos.Wallet.Web.Account     (AccountMode, getSKByAddr)
import           Pos.Wallet.Web.ClientTypes (CAccountMeta (..), CId, CWalletMeta (..),
                                             Wal)
import           Pos.Wallet.Web.State       (getWalletMeta)
import Pos.Wallet.Web.Error (WalletError (..))
import Pos.Util.Util (maybeThrow)

-- TODO: use `Data.Versions.SemVer` datatype for
-- accurate parsing and comparisons
currentBackupFormatVersion :: Text
currentBackupFormatVersion = "1.0.0"

newtype WalletMetaBackup = WalletMetaBackup CWalletMeta
newtype AccountMetaBackup = AccountMetaBackup CAccountMeta

data WalletBackup = WalletBackup
    { _wbSecretKey :: !EncryptedSecretKey
    , _wbMeta      :: !WalletMetaBackup
    , _wbAccounts  :: (HashMap Int AccountMetaBackup)
    }

makeLenses ''WalletBackup

data StateBackup = FullStateBackup [WalletBackup]

getWalletBackup :: AccountMode m => CId Wal -> m WalletBackup
getWalletBackup wId = do
    sk <- getSKByAddr wId
    meta <- maybeThrow (InternalError "Wallet have no meta") =<<
            getWalletMeta wId
    
