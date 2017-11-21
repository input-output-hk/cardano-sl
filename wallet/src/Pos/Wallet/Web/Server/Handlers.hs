{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet endpoints list

module Pos.Wallet.Web.Server.Handlers
       ( servantHandlers
       , servantHandlersWithSwagger
       ) where

import           Universum

import           Pos.Wallet.Web.Swagger.Spec (swaggerSpecForWalletApi)
import           Servant.API ((:<|>) ((:<|>)))
import           Servant.Server (Handler, Server, ServerT, hoistServer)
import           Servant.Swagger.UI (swaggerSchemaUIServer)

import           Pos.Update.Configuration (curSoftwareVersion)
import           Pos.Wallet.WalletMode (blockchainSlotDuration)
import           Pos.Wallet.Web.Account (GenSeed (RandomSeed))
import           Pos.Wallet.Web.Api (WalletApi, WalletSwaggerApi, walletApi)
import qualified Pos.Wallet.Web.Methods as M
import           Pos.Wallet.Web.Mode (MonadFullWalletWebMode)
import           Pos.Wallet.Web.Tracking (fixingCachedAccModifier)

servantHandlers
    :: MonadFullWalletWebMode ctx m
    => ServerT WalletApi m
servantHandlers =
     M.testResetAll
    :<|>
     M.dumpState
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

     M.newPayment
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

     M.redeemAda
    :<|>
     M.redeemAdaPaperVend
    :<|>

     M.reportingInitialized
    :<|>

     (blockchainSlotDuration <&> fromIntegral)
    :<|> pure curSoftwareVersion
    :<|>
     M.syncProgress
    :<|>
     M.localTimeDifference
    :<|>
     M.importWalletJSON
    :<|>
     M.exportWalletJSON
    :<|>
     M.getClientInfo

servantHandlersWithSwagger
    :: MonadFullWalletWebMode ctx m
    => (forall x. m x -> Handler x)
    -> Server WalletSwaggerApi
servantHandlersWithSwagger nat =
    hoistServer walletApi nat servantHandlers
   :<|>
    swaggerSchemaUIServer swaggerSpecForWalletApi
