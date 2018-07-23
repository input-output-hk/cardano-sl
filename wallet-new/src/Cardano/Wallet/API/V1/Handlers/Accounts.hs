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

handlers :: PassiveWalletLayer IO -> ServerT Accounts.API Handler
handlers w =  deleteAccount w
         :<|> getAccount w
         :<|> listAccounts w
         :<|> newAccount w
         :<|> updateAccount w

deleteAccount :: PassiveWalletLayer IO
              -> WalletId
              -> AccountIndex
              -> Handler NoContent
deleteAccount layer wId accIdx = do
    _ <- liftIO $ (WalletLayer.deleteAccount layer) wId accIdx
    error "unimplemented. See [CBR-342]"

-- | Fetches an 'Account' given its parent 'WalletId' and its index.
getAccount :: PassiveWalletLayer IO
           -> WalletId
           -> AccountIndex
           -> Handler (WalletResponse Account)
getAccount layer wId accIdx = do
    res <- liftIO $ (WalletLayer.getAccount layer) wId accIdx
    case res of
         Left e        -> throwM e
         Right account -> return $ single account

listAccounts :: PassiveWalletLayer IO
             -> WalletId
             -> RequestParams
             -> Handler (WalletResponse [Account])
listAccounts layer wId _params = do
    _ <- liftIO $ (WalletLayer.getAccounts layer) wId
    error "unimplemented. See [CBR-342]"
    -- case res of
    --      Left e         -> throwM e
    --      Right accounts ->
    --         respondWith params
    --             (NoFilters :: FilterOperations Account)
    --             (NoSorts :: SortOperations Account)
    --             (pure accounts)

newAccount :: PassiveWalletLayer IO
           -> WalletId
           -> NewAccount
           -> Handler (WalletResponse Account)
newAccount layer wId newAccountRequest = do
    res <- liftIO $ (WalletLayer.createAccount layer) wId newAccountRequest
    case res of
         Left e        -> throwM e
         Right account -> return $ single account

updateAccount :: PassiveWalletLayer IO
              -> WalletId
              -> AccountIndex
              -> AccountUpdate
              -> Handler (WalletResponse Account)
updateAccount layer wId accIdx accUpdate = do
    _ <- liftIO $ (WalletLayer.updateAccount layer) wId accIdx accUpdate
    error "unimplemented see [CBR-342]"
