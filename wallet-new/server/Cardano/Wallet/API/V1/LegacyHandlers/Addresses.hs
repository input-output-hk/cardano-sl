{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Cardano.Wallet.API.V1.LegacyHandlers.Addresses where

import           Universum

import           Pos.Crypto (emptyPassphrase)
import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.Methods as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types
import           Pos.Core (decodeTextAddress)

import           Servant
import           Test.QuickCheck (arbitrary, generate, vectorOf)

handlers
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => ServerT Addresses.API m
handlers =  listAddresses
       :<|> newAddress
       :<|> verifyAddress

listAddresses
    :: MonadIO m
    => RequestParams -> m (WalletResponse [Address])
listAddresses RequestParams {..} = do
    addresses <- liftIO $ generate (vectorOf 2 arbitrary)
    return WalletResponse {
              wrData = addresses
            , wrStatus = SuccessStatus
            , wrMeta = Metadata $ PaginationMetadata {
                        metaTotalPages = 1
                      , metaPage = 1
                      , metaPerPage = 20
                      , metaTotalEntries = 2
                      }
            }

newAddress
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => NewAddress -> m (WalletResponse WalletAddress)
newAddress NewAddress {..} = do
    let (V1 password) = fromMaybe (V1 emptyPassphrase) newaddrSpendingPassword
    accountId <- migrate (newaddrWalletId, newaddrAccountIndex)
    fmap single $ V0.newAddress V0.RandomSeed password accountId
              >>= migrate

-- | Verifies that an address is base58 decodable.
verifyAddress :: Monad m => Text -> m (WalletResponse AddressValidity)
verifyAddress address =
    case decodeTextAddress address of
        Right _ ->
            return $ single $ AddressValidity True
        Left _  ->
            return $ single $ AddressValidity False
