{-# LANGUAGE TypeFamilies #-}

-- | Wallet endpoints list

module Pos.Wallet.Web.Server.Handlers
       ( servantHandlers
       ) where

import           Universum

import           Pos.Communication             (SendActions (..))
import           Pos.Constants                 (curSoftwareVersion)
import           Pos.Wallet.WalletMode         (blockchainSlotDuration)
import           Pos.Wallet.Web.Account        (GenSeed (RandomSeed))
import           Pos.Wallet.Web.Api            (WalletApi)
import           Pos.Wallet.Web.Mode           (MonadWalletWebMode)
import qualified Pos.Wallet.Web.Server.Methods as M
import           Pos.Wallet.Web.Tracking       (fixingCachedAccModifier)
import           Servant.API                   ((:<|>) ((:<|>)))
import           Servant.Server                (ServerT)

servantHandlers
    :: MonadWalletWebMode m
    => SendActions m
    -> ServerT WalletApi m
servantHandlers sendActions =
     M.testResetAll
    :<|>

     M.getWallet
    :<|>
     M.getWallets
    :<|>
     M.newWallet
    :<|>
     M.updateWallet
    :<|>
     M.restoreWallet
    :<|>
     M.renameWSet
    :<|>
     M.deleteWallet
    :<|>
     M.importWallet
    :<|>
     M.changeWalletPassphrase
    :<|>

     fixingCachedAccModifier M.getAccount
    :<|>
     M.getAccounts
    :<|>
     M.updateAccount
    :<|>
     M.newAccount RandomSeed
    :<|>
     M.deleteAccount
    :<|>

     M.newAddress RandomSeed
    :<|>

     M.isValidAddress
    :<|>

     M.getUserProfile
    :<|>
     M.updateUserProfile
    :<|>

     M.newPayment sendActions
    :<|>
     M.getTxFee
    :<|>
     M.updateTransaction
    :<|>
     M.getHistoryLimited
    :<|>

     M.nextUpdate
    :<|>
     M.applyUpdate
    :<|>

     M.redeemAda sendActions
    :<|>
     M.redeemAdaPaperVend sendActions
    :<|>

     M.reportingInitialized
    :<|>
     M.reportingElectroncrash
    :<|>

     (blockchainSlotDuration <&> fromIntegral)
    :<|>
     pure curSoftwareVersion
    :<|>
     M.syncProgress
    :<|>
     M.importStateJSON
    :<|>
     M.exportStateJSON
