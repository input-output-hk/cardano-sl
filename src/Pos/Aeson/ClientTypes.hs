{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.ClientTypes
       (
       ) where

import           Data.Aeson.TH              (defaultOptions, deriveJSON, deriveToJSON)
import           Pos.Wallet.Web.ClientTypes (CAddress, CCurrency, CHash, CProfile,
                                             CTExMeta, CTType, CTx, CTxId, CTxMeta,
                                             CWallet, CWalletMeta, CWalletType)

deriveJSON defaultOptions ''CCurrency
deriveJSON defaultOptions ''CWalletType
deriveJSON defaultOptions ''CWalletMeta

deriveToJSON defaultOptions ''CHash
deriveToJSON defaultOptions ''CAddress
deriveToJSON defaultOptions ''CTxId
deriveToJSON defaultOptions ''CWallet
deriveToJSON defaultOptions ''CProfile
deriveToJSON defaultOptions ''CTx
deriveToJSON defaultOptions ''CTxMeta
deriveToJSON defaultOptions ''CTType
deriveToJSON defaultOptions ''CTExMeta
