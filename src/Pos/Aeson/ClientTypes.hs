{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.ClientTypes
       (
       ) where

import           Data.Aeson.TH              (defaultOptions, deriveJSON, deriveToJSON)
import           Pos.Util.BackupPhrase      (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CHash, CProfile,
                                             CProfile, CTExMeta, CTType, CTx, CTxId,
                                             CTxMeta, CWallet, CWalletInit, CWalletMeta,
                                             CWalletType, NotifyEvent)
import           Pos.Wallet.Web.Error       (WalletError)

deriveJSON defaultOptions ''CCurrency
deriveJSON defaultOptions ''CWalletType
deriveJSON defaultOptions ''CWalletMeta
deriveJSON defaultOptions ''CWalletInit
deriveJSON defaultOptions ''CTxMeta
deriveJSON defaultOptions ''CProfile
deriveJSON defaultOptions ''BackupPhrase

deriveToJSON defaultOptions ''NotifyEvent
deriveToJSON defaultOptions ''WalletError
deriveToJSON defaultOptions ''CHash
deriveToJSON defaultOptions ''CAddress
deriveToJSON defaultOptions ''CTxId
deriveToJSON defaultOptions ''CWallet
deriveToJSON defaultOptions ''CTx
deriveToJSON defaultOptions ''CTType
deriveToJSON defaultOptions ''CTExMeta
