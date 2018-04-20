{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Wallet.API.V1.LegacyHandlers.Addresses where

import           Universum

import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.IxSet.Typed as IxSet
import qualified Data.List as List
import           Servant


import           Pos.Core (decodeTextAddress)
import           Pos.Crypto (emptyPassphrase)
import qualified Pos.Txp as V0 (withTxpLocalData)
import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.ClientTypes as V0
import           Pos.Wallet.Web.ClientTypes.Types (CAccount (..))
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.Methods.Logic as V0 (getMempoolSnapshot, getWAddress)
import qualified Pos.Wallet.Web.State as V0 (askWalletSnapshot)
import           Pos.Wallet.Web.State.State (WalletSnapshot, askWalletDB, getWalletSnapshot)
import qualified Pos.Wallet.Web.State.State as V0State
import           Pos.Wallet.Web.State.Storage (getWalletAddresses)
import qualified Pos.Wallet.Web.State.Storage as V0
import qualified Pos.Wallet.Web.Tracking as V0 (txMempoolToModifier)
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)

import           Cardano.Wallet.API.Indices (IxSet')
import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types

handlers
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => ServerT Addresses.API m
handlers =  listAddresses
       :<|> newAddress
       :<|> getAddress

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
    :: forall ctx m. (MonadThrow m, V0.MonadWalletLogic ctx m)
    => RequestParams
    -> m (WalletResponse [WalletAddress])
listAddresses params = do

    wdb <- askWalletDB
    ws  <- getWalletSnapshot wdb

    let allAddresses = runStreamAddresses ws

    respondWith params (NoFilters :: FilterOperations WalletAddress)
                       (NoSorts   :: SortOperations   WalletAddress)
                       allAddresses
  where
    -- | Should improve performance, stream fusion ultra super nuclear
    -- fission... Insert cool word of coice.
    runStreamAddresses :: WalletSnapshot -> m (IxSet' WalletAddress)
    runStreamAddresses ws =
        runConduit   $ CL.sourceList (getWalletAddresses ws)
                    .| CL.map Just
                    .| CL.concatMapM V0.getAccounts
                    .| CL.concatMap caAddresses
                    .| CL.mapM migrate
                    .| CL.fold (\x a -> IxSet.updateIx (addrId a) a x) IxSet.empty

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
        addrInfoMatchesAddr ::  V0.AddressInfo -> Bool
        addrInfoMatchesAddr addrInfo =
            addr == V0._wamAddress (V0.adiWAddressMeta addrInfo)

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
            let accId = adiWAddressMeta ^. V0.wamAccount
            mps <- V0.withTxpLocalData V0.getMempoolSnapshot
            accMod <- V0.txMempoolToModifier ws mps . eskToWalletDecrCredentials =<< V0.findKey accId
            let caddr = V0.getWAddress ws accMod adiWAddressMeta
            single <$> migrate caddr
