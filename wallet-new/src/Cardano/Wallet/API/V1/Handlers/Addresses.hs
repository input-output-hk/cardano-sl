{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Wallet.API.V1.Handlers.Addresses where

import           Universum

import           Servant

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer,
                     PassiveWalletLayer (..), walletPassiveLayer)

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Types

import           Pos.Core.Mockable.Production (Production, runProduction)

handlers :: ActiveWalletLayer Production -> ServerT Addresses.API Handler
handlers w =  listAddresses
         :<|> newAddress (walletPassiveLayer w)
         :<|> getAddress

listAddresses :: RequestParams -> Handler (WalletResponse [WalletAddress])
listAddresses _params = error "Unimplemented - See [CBR-227]."

newAddress :: PassiveWalletLayer Production
           -> NewAddress
           -> Handler (WalletResponse WalletAddress)
newAddress pwl newAddressRequest = do
    res <- liftIO $ runProduction $ (_pwlCreateAddress pwl) newAddressRequest
    case res of
         Left err      -> throwM err
         Right newAddr -> return $ single (WalletAddress (V1 newAddr) False False)

-- | Verifies that an address is base58 decodable.
getAddress :: Text -> Handler (WalletResponse WalletAddress)
getAddress _addrText = error "Unimplemented - See [CBR-227]."
