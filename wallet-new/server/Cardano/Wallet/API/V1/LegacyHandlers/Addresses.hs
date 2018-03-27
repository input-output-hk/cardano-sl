{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Wallet.API.V1.LegacyHandlers.Addresses where

import           Universum

import qualified Data.List as List
import           Pos.Crypto (emptyPassphrase)
import qualified Pos.Txp as V0 (withTxpLocalData)
import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.ClientTypes as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.Methods.Logic as V0 (getMempoolSnapshot, getWAddress)
import qualified Pos.Wallet.Web.State as V0 (askWalletSnapshot)
import qualified Pos.Wallet.Web.State.State as V0State
import qualified Pos.Wallet.Web.State.Storage as V0
import qualified Pos.Wallet.Web.Tracking as V0 (txMempoolToModifier)

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Errors
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
       :<|> getAddress

listAddresses
    :: MonadIO m
    => RequestParams -> m (WalletResponse [WalletAddress])
listAddresses RequestParams {..} = do
    -- TODO(matt.parsons):  Fix as part of CSL-2146
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
    -> m (WalletResponse WalletAddress)
getAddress addrText = do
    addr <- either
        (throwM . InvalidAddressFormat)
        pure
        (decodeTextAddress addrText)

    ws <- V0.askWalletSnapshot

    let
        addrId :: V0.CId V0.Addr
        addrId =
            V0.addressToCId addr

        addrInfoMatchesAddr ::  V0.AddressInfo -> Bool
        addrInfoMatchesAddr addrInfo =
            addrId == V0.cwamId (V0.adiCWAddressMeta addrInfo)

        getAddresses :: V0.CId V0.Wal -> [V0.AddressInfo]
        getAddresses = V0State.getWAddresses ws V0.Ever

        minfo :: Maybe (V0.CWalletMeta, V0.AddressInfo)
        minfo =
            asum
            . map (\wid ->
                (,) <$> V0State.getWalletMeta ws wid
                    <*> List.find addrInfoMatchesAddr (getAddresses wid)
                )
            . V0.getWalletAddresses
            $ ws

    case minfo of
        Nothing ->
            throwM AddressNotFound
        Just (_walletMeta, V0.AddressInfo{..}) -> do
            let accId = V0.AccountId
                    (V0.cwamWId adiCWAddressMeta)
                    (V0.cwamAccountIndex adiCWAddressMeta)
            mps <- V0.withTxpLocalData V0.getMempoolSnapshot
            accMod <- V0.txMempoolToModifier ws mps =<< V0.findKey accId
            caddr <- V0.getWAddress
                ws
                accMod
                adiCWAddressMeta
            single <$> migrate caddr
