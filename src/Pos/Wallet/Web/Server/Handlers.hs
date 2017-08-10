{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Handlers
       ( servantHandlers
       ) where

import qualified Pos.Wallet.Web.Server.Methods as M

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

     M.fixingCachedAccModifier getAccount
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
     M.pure curSoftwareVersion
    :<|>
     M.syncProgress
    :<|>
     M.importStateJSON
    :<|>
     M.exportStateJSON
