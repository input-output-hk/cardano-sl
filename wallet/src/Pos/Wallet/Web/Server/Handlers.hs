{-# LANGUAGE TypeFamilies #-}

-- | Wallet endpoints list

module Pos.Wallet.Web.Server.Handlers
       ( servantHandlers
       , servantHandlersWithSwagger
       ) where

import           Universum

import           Pos.Wallet.Web.Swagger.Spec (swaggerSpecForWalletApi)
import           Servant.API                 ((:<|>) ((:<|>)))
import           Servant.Server              (Handler, Server, ServerT)
import           Servant.Swagger.UI          (swaggerSchemaUIServer)
import           Servant.Utils.Enter         ((:~>) (..), enter)

import           Pos.Communication           (SendActions (..))
import           Pos.Constants               (curSoftwareVersion)
import           Pos.Wallet.WalletMode       (blockchainSlotDuration)
import           Pos.Wallet.Web.Account      (GenSeed (RandomSeed))
import           Pos.Wallet.Web.Api          (WalletApi, WalletSwaggerApi)
import qualified Pos.Wallet.Web.Methods      as M
import           Pos.Wallet.Web.Mode         (MonadWalletWebMode)
import           Pos.Wallet.Web.Tracking     (fixingCachedAccModifier)

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
     M.postponeUpdate
    :<|>
     M.applyUpdate
    :<|>

     M.redeemAda sendActions
    :<|>
     M.redeemAdaPaperVend sendActions
    :<|>

     M.reportingInitialized
    :<|>

     (blockchainSlotDuration <&> fromIntegral)
    :<|>
     pure curSoftwareVersion
    :<|>
     M.syncProgress
    :<|>
     M.importWalletJSON
    :<|>
     M.exportWalletJSON

servantHandlersWithSwagger
    :: MonadWalletWebMode m
    => SendActions m
    -> (m :~> Handler)
    -> Server WalletSwaggerApi
servantHandlersWithSwagger sendActions nat =
    nat `enter` servantHandlers sendActions
   :<|>
    -- doesn't work for arbitrary monad, so we have to 'enter' above
    swaggerSchemaUIServer swaggerSpecForWalletApi

