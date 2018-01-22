{-# LANGUAGE TypeFamilies #-}

-- | Wallet endpoints list

module Pos.Wallet.Web.Server.Handlers
       ( servantHandlers
       ) where

import           Universum

import           Pos.Communication          (SendActions (..))
import           Pos.Update.Configuration   (curSoftwareVersion)
import           Pos.Wallet.WalletMode      (blockchainSlotDuration)
import           Pos.Wallet.Web.Account     (GenSeed (RandomSeed))
import           Pos.Wallet.Web.Api         (WalletApi)
import qualified Pos.Wallet.Web.Methods     as M
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.State.State (getWalletSnapshot)
import           Servant.API                ((:<|>) ((:<|>)))
import           Servant.Server             (ServerT)

servantHandlers
    :: MonadWalletWebMode m
    => SendActions m
    -> ServerT WalletApi m
servantHandlers sendActions =
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
     M.getAccount
    :<|>
     M.getAccounts
    :<|>
     M.updateAccount
    :<|>
     M.newAccount RandomSeed
    :<|>
     M.deleteAccount
    :<|> (\passPhrase accId -> do
             ws <- getWalletSnapshot
             M.newAddress ws RandomSeed passPhrase accId
         )
    :<|>
     M.isValidAddress
    :<|>
     M.getUserProfile
    :<|>
     M.updateUserProfile
    :<|>
     M.newPayment sendActions
    :<|>
     M.newPaymentBatch sendActions
    :<|>
     M.getTxFee
    :<|>
     M.cancelAllApplyingPtxs
    :<|>
     M.cancelOneApplyingPtx
    :<|>
     M.updateTransaction
    :<|>
     M.getHistoryLimited
    :<|>
     M.gatherPendingTxsSummary
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
    :<|> pure curSoftwareVersion
    :<|>
     M.syncProgress
    :<|>
     M.importWalletJSON
    :<|>
     M.exportWalletJSON

    :<|>
     M.getClientInfo
