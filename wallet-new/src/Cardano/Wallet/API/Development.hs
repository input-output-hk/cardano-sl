{-# OPTIONS_GHC -fno-warn-orphans #-}
-- orphan instance is proivded for V1 WalletStateSnapshot, as it is only
-- exposed as an octetstream anyway

module Cardano.Wallet.API.Development where

import           Universum

import           Cardano.Wallet.API.Response (ValidJSON, WalletResponse)
import           Cardano.Wallet.API.Types (Tags)
import           Cardano.Wallet.API.V1.Types (V1 (..))
import           Data.Aeson
import           Data.Swagger (NamedSchema (..), ToSchema (..))
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot)
import           Servant
import           Servant.API.ContentTypes (OctetStream)

-- the ToSchema instance that was being generated for this type is invalid,
-- so we make one here an dhide it behind the WalletStateSnapshot
instance ToSchema (V1 WalletStateSnapshot) where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "V1WalletStateSnapshot") mempty

instance ToJSON (V1 WalletStateSnapshot) where
    toJSON (V1 x) = toJSON x

type API
    = Tags '["Development"] :>
    (
         "dump-wallet-state"  :> Summary "Dump wallet state."
                              :> Get '[OctetStream] (WalletResponse (V1 WalletStateSnapshot))
    :<|> "secret-keys"        :> Summary "Clear wallet state and delete all the secret keys."
                              :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "fail"               :> Summary "Throw a generic error"
                              :> GetNoContent '[ValidJSON] NoContent
    )
