{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Cardano.Wallet.API.V1.Handlers.Addresses where

import           Universum

import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Types

import           Servant

handlers :: Server Addresses.API
handlers =  listAddresses
       :<|> newAddress

listAddresses :: Maybe Page
              -> Maybe PerPage
              -> Maybe Bool
              -> Maybe Text
              -> Handler (OneOf [Address] (ExtendedResponse [Address]))
listAddresses _ _ mbExtended _ =
  case mbExtended of
    Just True  -> return $ OneOf $ Right $
      ExtendedResponse {
        ext_data = [Address "deadBeef", Address "123AABBCC"]
      , ext_meta = Metadata {
          meta_total_pages = 1
        , meta_page = 1
        , meta_per_page = 20
        , meta_total_entries = 2
      }
      }
    _ -> return $ OneOf $ Left  [Address "deadBeef", Address "123AABBCC"]

newAddress :: Address -> Handler Address
newAddress a = return a
