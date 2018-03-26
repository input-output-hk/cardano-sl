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

import           Pos.Wallet.Web.State.Storage (getWalletAddresses)
import           Pos.Wallet.Web.State.State (getWalletSnapshot, askWalletDB)
import           Pos.Wallet.Web.ClientTypes.Types (CAccount (..))

import           Servant

import qualified Data.IxSet.Typed as IxSet

handlers
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => ServerT Addresses.API m
handlers =  listAddresses
       :<|> newAddress
       :<|> verifyAddress

-- | This is quite slow. What happens when we have 50k addresses?
-- TODO(ks): One idea I have is to persist the length of the
-- addresses and send that to `respondWith`,
-- while we could lazily fetch the data (and have better performance)
-- we need to show what the current page contains?
-- In other words, we wouldn't send everything to `respondWith`.
-- Another idea is to use actual paging to reduce the footprint of all
-- these calls. The flaw with this is that we need to think about deletion,
-- but I have an idea or two how that can be fixed.
listAddresses
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => RequestParams
    -> m (WalletResponse [WalletAddress])
listAddresses params = do

    wdb <- askWalletDB
    ws  <- getWalletSnapshot wdb

    let walletAddresses   = getWalletAddresses ws
    allCAccounts         <- concatMapM (V0.getAccounts . Just) walletAddresses
    let allCAddresses     = concatMap caAddresses allCAccounts
    let allAddresses      = migrate allCAddresses

    respondWith params (NoFilters :: FilterOperations WalletAddress)
                       (NoSorts   :: SortOperations   WalletAddress)
                       (IxSet.fromList <$> allAddresses)

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
