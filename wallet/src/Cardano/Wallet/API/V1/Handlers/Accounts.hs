{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Wallet.API.V1.Handlers.Accounts where

import           Universum

import           Servant

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

handlers :: PassiveWalletLayer IO -> ServerT Accounts.API Handler
handlers w =  deleteAccount w
         :<|> getAccount    w
         :<|> listAccounts  w
         :<|> newAccount    w
         :<|> updateAccount w
         :<|> getAccountAddresses w
         :<|> getAccountBalance w

deleteAccount :: PassiveWalletLayer IO
              -> WalletId
              -> AccountIndex
              -> Handler NoContent
deleteAccount layer wId accIdx = do
    res <- liftIO $ WalletLayer.deleteAccount layer wId accIdx
    case res of
         Left e   -> throwM e
         Right () -> return NoContent

-- | Fetches an 'Account' given its parent 'WalletId' and its index.
getAccount :: PassiveWalletLayer IO
           -> WalletId
           -> AccountIndex
           -> Handler (APIResponse Account)
getAccount layer wId accIdx = do
    res <- liftIO $ WalletLayer.getAccount layer wId accIdx
    case res of
         Left e        -> throwM e
         Right account -> return $ single account

listAccounts :: PassiveWalletLayer IO
             -> WalletId
             -> RequestParams
             -> Handler (APIResponse [Account])
listAccounts layer wId params = do
    res <- liftIO $ WalletLayer.getAccounts layer wId
    case res of
         Left e         -> throwM e
         Right accounts ->
            respondWith params
                (NoFilters :: FilterOperations '[] Account)
                (NoSorts :: SortOperations Account)
                (pure accounts)

newAccount :: PassiveWalletLayer IO
           -> WalletId
           -> NewAccount
           -> Handler (APIResponse Account)
newAccount layer wId newAccountRequest = do
    res <- liftIO $ WalletLayer.createAccount layer wId newAccountRequest
    case res of
         Left e        -> throwM e
         Right account -> return $ single account

updateAccount :: PassiveWalletLayer IO
              -> WalletId
              -> AccountIndex
              -> AccountUpdate
              -> Handler (APIResponse Account)
updateAccount layer wId accIdx updateRequest = do
    res <- liftIO $ WalletLayer.updateAccount layer wId accIdx updateRequest
    case res of
         Left e -> throwM e
         Right updatedAccount ->
             return $ single updatedAccount

getAccountAddresses
    :: PassiveWalletLayer IO
    -> WalletId
    -> AccountIndex
    -> RequestParams
    -> FilterOperations '[V1 Address] WalletAddress
    -> Handler (APIResponse AccountAddresses)
getAccountAddresses layer wId accIdx pagination filters = do
    -- NOTE: Many of the Servant handlers have the following structure:
    --
    -- 1. Get some @values :: IxSet KernelType@ from the wallet layer
    -- 2. Translate that to some @values' :: IxSet SomeV1Type@
    -- 3. Use @respondWith someFilterParams values'@ to filter it and return it
    --
    -- This is an anti-pattern: constructing `theData` would mean constructing a
    -- /new/ `IxSet` of V1 types from the existing `IxSet` containing kernel
    -- types, reconstructing any indices etc. Moreover, it would do so on each
    -- request! Instead we should work only with the /existing/ `IxSet`s and
    -- only translate to `V1` types at the very end (for the few items that
    -- we actually return from the handler). I.e., we should
    --
    -- 1. Get some @values :: IxSet KernelType@ from the wallet layer
    -- 2. Use @respondWith someFilterParams values@ to filter it and get a
    --    handful of values out that we actually return to the user
    -- 3. Translate those to @V1@ types
    --
    -- This is what we do here, but we don't yet do it for the other handlers.
    -- See <https://iohk.myjetbrains.com/youtrack/issue/CBR-356>
    -- and <https://iohk.myjetbrains.com/youtrack/issue/CBR-389>.
    res <- liftIO $ WalletLayer.getAccountAddresses layer wId accIdx pagination filters
    case res of
         Left e      -> throwM e
         Right addrs -> return $ AccountAddresses <$> addrs

getAccountBalance
    :: PassiveWalletLayer IO
    -> WalletId
    -> AccountIndex
    -> Handler (APIResponse AccountBalance)
getAccountBalance layer wId accIdx = do
    res <- liftIO $ WalletLayer.getAccountBalance layer wId accIdx
    case res of
        Left e        -> throwM e
        Right balance -> return $ single balance
