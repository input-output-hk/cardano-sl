{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Cardano.Wallet.API.V1.LegacyHandlers.Addresses where

import           Universum

import           Pos.Crypto (emptyPassphrase)
import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0
import qualified Pos.Wallet.Web.State.Storage as V0
import Control.Lens (at)

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.API.V1.Errors
import           Pos.Core (decodeTextAddress)

import           Servant
import           Test.QuickCheck (arbitrary, generate, vectorOf)

handlers
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => ServerT Addresses.API m
handlers =  listAddresses
       :<|> newAddress
       :<|> getAddress

listAddresses
    :: MonadIO m
    => RequestParams -> m (WalletResponse [Address])
listAddresses RequestParams {..} = do
    -- TODO: lmao
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
getAddress
    :: (MonadThrow m , V0.MonadWalletLogic ctx m)
    => Text
    -> m (WalletResponse AddressInfo)
getAddress addrText = do
    addr <- either
        (throwM . InvalidAddressFormat)
        pure
        (decodeTextAddress addrText)

    ss <- V0.askWalletSnapshot

    let mOurAddr = ss ^? V0.wsBalances . at addr

    case mOurAddr of
        Nothing ->
            throwM err404 -- TODO: make this a real error in the type
        Just balance ->
            pure $ undefined "return the real deal"

-- ss has:
-- - wsWalletInfos (Map (CId Wal) (WalletInfo))
--   - WalletInfo has CWalletMeta (deadend)
-- - wsAccountInfos (Map AccountId AccountInfo)
--   - aiMeta (just a name)
--   - aiAddresses (CAddresses ~ Map (CId Addr) AddressInfo
--     - AddressInfo has:
--       - CWAddressMeta has:
--         - CId Wal
--         - index 1+2 for derivation key
--         - CID Addr
--       - AddressSortingKey-
-- - wsBalances (WalletBalances ~ AddrCoinMap ~ Map Address Coin
--   - this has all the addresses that belong to this wallet, pointing to
--     their balance!


