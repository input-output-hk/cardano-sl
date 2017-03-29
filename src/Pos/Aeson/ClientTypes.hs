{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.ClientTypes
       (
       ) where

import           Data.Aeson.TH              (defaultOptions, deriveJSON, deriveToJSON)
import           Pos.Core.Types             (BlockVersion (..), SoftwareVersion (..))
import           Pos.Util.BackupPhrase      (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes (CAccount, CAccountAddress, CAccountRedeem,
                                             CAddress, CCurrency, CHash, CInitialized,
                                             CProfile, CProfile, CTExMeta, CTType, CTx,
                                             CTxId, CTxMeta, CUpdateInfo, CWallet,
                                             CWalletAddress, CWalletInit, CWalletMeta,
                                             CWalletSet, CWalletSetAddress,
                                             CWalletSetInit, CWalletSetMeta, CWalletType,
                                             NotifyEvent, SyncProgress)
import           Pos.Wallet.Web.Error       (WalletError)

deriveJSON defaultOptions ''CCurrency
deriveJSON defaultOptions ''CWalletSetAddress
deriveJSON defaultOptions ''CWalletAddress
deriveJSON defaultOptions ''CAccountAddress
deriveJSON defaultOptions ''CWalletType
deriveJSON defaultOptions ''CWalletMeta
deriveJSON defaultOptions ''CWalletInit
deriveJSON defaultOptions ''CAccountRedeem
deriveJSON defaultOptions ''CWalletSetMeta
deriveJSON defaultOptions ''CWalletSetInit
deriveJSON defaultOptions ''CTxMeta
deriveJSON defaultOptions ''CProfile
deriveJSON defaultOptions ''BackupPhrase
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CHash
deriveJSON defaultOptions ''CInitialized

deriveToJSON defaultOptions ''SyncProgress
deriveToJSON defaultOptions ''NotifyEvent
deriveToJSON defaultOptions ''WalletError
deriveToJSON defaultOptions ''CTxId
deriveToJSON defaultOptions ''CAccount
deriveToJSON defaultOptions ''CWallet
deriveToJSON defaultOptions ''CWalletSet
deriveToJSON defaultOptions ''CTx
deriveToJSON defaultOptions ''CTType
deriveToJSON defaultOptions ''CTExMeta
deriveToJSON defaultOptions ''SoftwareVersion
deriveToJSON defaultOptions ''BlockVersion
deriveToJSON defaultOptions ''CUpdateInfo
