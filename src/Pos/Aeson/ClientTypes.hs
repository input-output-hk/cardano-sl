{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.ClientTypes
       (
       ) where

import           Data.Aeson.TH              (defaultOptions, deriveJSON, deriveToJSON)
import           Pos.Types.Version          (BlockVersion (..), SoftwareVersion (..))
import           Pos.Util.BackupPhrase      (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CHash, CProfile,
                                             CProfile, CTExMeta, CTType, CTx, CTxId,
                                             CTxMeta, CUpdateInfo, CWallet, CWalletInit,
                                             CWalletMeta, CWalletRedeem, CWalletType,
                                             NotifyEvent, SyncProgress)
import           Pos.Wallet.Web.Error       (WalletError)

deriveJSON defaultOptions ''CCurrency
deriveJSON defaultOptions ''CWalletType
deriveJSON defaultOptions ''CWalletMeta
deriveJSON defaultOptions ''CWalletInit
deriveJSON defaultOptions ''CWalletRedeem
deriveJSON defaultOptions ''CTxMeta
deriveJSON defaultOptions ''CProfile
deriveJSON defaultOptions ''BackupPhrase
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CHash

deriveToJSON defaultOptions ''SyncProgress
deriveToJSON defaultOptions ''NotifyEvent
deriveToJSON defaultOptions ''WalletError
deriveToJSON defaultOptions ''CTxId
deriveToJSON defaultOptions ''CWallet
deriveToJSON defaultOptions ''CTx
deriveToJSON defaultOptions ''CTType
deriveToJSON defaultOptions ''CTExMeta
deriveToJSON defaultOptions ''SoftwareVersion
deriveToJSON defaultOptions ''BlockVersion
deriveToJSON defaultOptions ''CUpdateInfo
