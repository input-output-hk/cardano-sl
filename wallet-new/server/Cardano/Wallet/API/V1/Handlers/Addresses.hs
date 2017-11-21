{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Cardano.Wallet.API.V1.Handlers.Addresses where

import           Universum

import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Types

import           Servant
import           Test.QuickCheck (arbitrary, generate, vectorOf)

handlers :: Server Addresses.API
handlers =  listAddresses
       :<|> newAddress

listAddresses :: PaginationParams
              -> Handler (OneOf [Address] (ExtendedResponse [Address]))
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

newAddress :: Address -> Handler Address
newAddress a = return a
