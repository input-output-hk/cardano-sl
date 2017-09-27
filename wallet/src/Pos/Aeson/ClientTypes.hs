module Pos.Aeson.ClientTypes
       (
       ) where

import           Data.Aeson.TH                (defaultOptions, deriveJSON, deriveToJSON)
import           Pos.Core.Types               (SoftwareVersion (..))
import           Pos.Util.BackupPhrase        (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes   (Addr, CAccount, CAccountId, CAccountInit,
                                               CAccountMeta, CAddress, CCoin, CHash, CId,
                                               CInitialized, CInitialized,
                                               CPaperVendWalletRedeem, CProfile, CProfile,
                                               CPtxCondition, CTExMeta, CTx, CTxId,
                                               CTxMeta, CUpdateInfo, CWAddressMeta,
                                               CWallet, CWalletAssurance, CWalletInit,
                                               CWalletMeta, CWalletRedeem, SyncProgress,
                                               Wal)
import           Pos.Wallet.Web.Error         (WalletError)
import           Pos.Wallet.Web.Sockets.Types (NotifyEvent)

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
deriveJSON defaultOptions ''Wal
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
deriveToJSON defaultOptions ''CPtxCondition
deriveToJSON defaultOptions ''CTx
deriveToJSON defaultOptions ''CTExMeta
deriveToJSON defaultOptions ''SoftwareVersion
deriveToJSON defaultOptions ''CUpdateInfo
