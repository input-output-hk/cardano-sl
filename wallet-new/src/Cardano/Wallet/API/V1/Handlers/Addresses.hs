{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Wallet.API.V1.Handlers.Addresses where

import           Universum

import           Servant

import           Cardano.Wallet.WalletLayer.Types (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer.Types as WalletLayer

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Types


handlers :: PassiveWalletLayer IO -> ServerT Addresses.API Handler
handlers w =  listAddresses w
         :<|> newAddress w
         :<|> getAddress w

listAddresses :: PassiveWalletLayer IO
              -> RequestParams -> Handler (WalletResponse [WalletAddress])
listAddresses pwl params = do
    addrs <- liftIO $ WalletLayer.getAddresses pwl params
    return $ fromSlice (rpPaginationParams params) addrs

newAddress :: PassiveWalletLayer IO
           -> NewAddress
           -> Handler (WalletResponse WalletAddress)
newAddress pwl newAddressRequest = do
    res <- liftIO $ WalletLayer.createAddress pwl newAddressRequest
    case res of
         Left err      -> throwM err
         Right newAddr -> return $ single newAddr

-- | Validates an input 'Text' following these simple principles:
--
-- 1. The input text must be parseable into a Cardano Address;
-- 2. The input text must be a valid, @local@ 'Address', i.e. an 'Address'
--    known to this wallet.
getAddress :: PassiveWalletLayer IO
           -> Text
           -> Handler (WalletResponse WalletAddress)
getAddress pwl addressRaw = do
    res <- liftIO $ WalletLayer.validateAddress pwl addressRaw
    case res of
         Left err   -> throwM err
         Right addr -> return $ single addr
