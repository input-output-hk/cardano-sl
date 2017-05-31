{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.ClientTypes
       (
       ) where

import           Data.Aeson.TH              (defaultOptions, deriveJSON, deriveToJSON)
import           Pos.Core.Types             (BlockVersion (..), SoftwareVersion (..))
import           Pos.Util.BackupPhrase      (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes (Addr, CAddress, CWAddressMeta, CId,
                                             CCoin, CHash, CInitialized, CInitialized,
                                             CPaperVendWalletRedeem, CProfile, CProfile,
                                             CTExMeta, CTx, CTxId, CTxMeta, CUpdateInfo,
                                             CAccount, CAccountId, CAccountInit,
                                             CAccountMeta, CWalletRedeem, CWallet,
                                             CWalletAssurance, CWalletInit,
                                             CWalletMeta, NotifyEvent, SyncProgress,
                                             WS)
import           Pos.Wallet.Web.Error       (WalletError)

deriveJSON defaultOptions ''CAccountId
deriveJSON defaultOptions ''CWAddressMeta
deriveJSON defaultOptions ''CWalletAssurance
deriveJSON defaultOptions ''CAccountMeta
deriveJSON defaultOptions ''CAccountInit
deriveJSON defaultOptions ''CWalletRedeem
deriveJSON defaultOptions ''CWalletMeta
deriveJSON defaultOptions ''CWalletInit
deriveJSON defaultOptions ''CPaperVendWalletRedeem
deriveJSON defaultOptions ''CTxMeta
deriveJSON defaultOptions ''CProfile
deriveJSON defaultOptions ''BackupPhrase
deriveJSON defaultOptions ''CId
deriveJSON defaultOptions ''WS
deriveJSON defaultOptions ''Addr
deriveJSON defaultOptions ''CHash
deriveJSON defaultOptions ''CInitialized

deriveToJSON defaultOptions ''CCoin
deriveToJSON defaultOptions ''SyncProgress
deriveToJSON defaultOptions ''NotifyEvent
deriveToJSON defaultOptions ''WalletError
deriveToJSON defaultOptions ''CTxId
deriveToJSON defaultOptions ''CAddress
deriveToJSON defaultOptions ''CAccount
deriveToJSON defaultOptions ''CWallet
deriveToJSON defaultOptions ''CTx
deriveToJSON defaultOptions ''CTExMeta
deriveToJSON defaultOptions ''SoftwareVersion
deriveToJSON defaultOptions ''BlockVersion
deriveToJSON defaultOptions ''CUpdateInfo
