{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Wallet.API.V1.Handlers.Accounts where

import           Universum

import           Servant

import           Cardano.Wallet.WalletLayer.Types (PassiveWalletLayer (..))
import qualified Cardano.Wallet.WalletLayer.Types as WalletLayer

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Types

import           Pos.Core.Mockable.Production (Production, runProduction)

handlers :: PassiveWalletLayer Production -> ServerT Accounts.API Handler
handlers w =  deleteAccount w
         :<|> getAccount w
         :<|> listAccounts w
         :<|> newAccount w
         :<|> updateAccount w

deleteAccount :: PassiveWalletLayer Production
              -> WalletId
              -> AccountIndex
              -> Handler NoContent
deleteAccount layer wId accIdx = do
    _ <- liftIO $ runProduction $ (WalletLayer.deleteAccount layer) wId accIdx
    error "unimplemented. See [CBR-342]"

getAccount :: PassiveWalletLayer Production
           -> WalletId
           -> AccountIndex
           -> Handler (WalletResponse Account)
getAccount layer wId accIdx = do
    _ <- liftIO $ runProduction $ (WalletLayer.getAccount layer) wId accIdx
    error "unimplemented. See [CBR-342]"

listAccounts :: PassiveWalletLayer Production
             -> WalletId
             -> RequestParams
             -> Handler (WalletResponse [Account])
listAccounts layer wId _params = do
    _ <- liftIO $ runProduction $ (WalletLayer.getAccounts layer) wId
    error "unimplemented. See [CBR-342]"
    -- case res of
    --      Left e         -> throwM e
    --      Right accounts ->
    --         respondWith params
    --             (NoFilters :: FilterOperations Account)
    --             (NoSorts :: SortOperations Account)
    --             (pure accounts)

newAccount :: PassiveWalletLayer Production
           -> WalletId
           -> NewAccount
           -> Handler (WalletResponse Account)
newAccount layer wId newAccountRequest = do
    res <- liftIO $ runProduction $ (WalletLayer.createAccount layer) wId newAccountRequest
    case res of
         Left e        -> throwM e
         Right account -> return $ single account

updateAccount :: PassiveWalletLayer Production
              -> WalletId
              -> AccountIndex
              -> AccountUpdate
              -> Handler (WalletResponse Account)
updateAccount layer wId accIdx accUpdate = do
    _ <- liftIO $ runProduction $ (WalletLayer.updateAccount layer) wId accIdx accUpdate
    error "unimplemented see [CBR-342]"
