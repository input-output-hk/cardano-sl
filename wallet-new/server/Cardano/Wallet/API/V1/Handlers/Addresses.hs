{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Cardano.Wallet.API.V1.Handlers.Addresses where

import           Universum

import           Pos.Crypto (emptyPassphrase)
import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.Methods as V0

import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types

import           Servant
import           Test.QuickCheck (arbitrary, generate, vectorOf)

handlers
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => ServerT Addresses.API m
handlers =  listAddresses
       :<|> newAddress

listAddresses
    :: MonadIO m
    => PaginationParams -> m (OneOf [Address] (ExtendedResponse [Address]))
listAddresses PaginationParams {..} = do
    addresses <- liftIO $ generate (vectorOf 2 arbitrary)
    case ppResponseFormat of
        Extended -> return $ OneOf $ Right $
            ExtendedResponse {
                extData = addresses
              , extMeta = Metadata {
                      metaTotalPages = 1
                    , metaPage = 1
                    , metaPerPage = 20
                    , metaTotalEntries = 2
                    }
              }
        _ -> return $ OneOf $ Left addresses

newAddress
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => NewAddress -> m WalletAddress
newAddress NewAddress {..} = do
    let password = fromMaybe emptyPassphrase newaddrSpendingPassword
    accountId <- migrate (newaddrWalletId, newaddrAccountId)
    V0.newAddress V0.RandomSeed password accountId
        >>= migrate
