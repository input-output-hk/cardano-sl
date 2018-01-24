{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.Balances
       ( getOwnUtxos
       , getOwnUtxoForPk
       , getBalanceFromUtxo
       ) where

import           Universum

import qualified Data.HashSet         as HS
import           Data.List            (partition)
import qualified Data.Map             as M

import           Pos.Core             (Address (..), Coin, IsBootstrapEraAddr (..),
                                       isRedeemAddress, makePubKeyAddress)
import           Pos.Crypto           (PublicKey)
import           Pos.DB               (MonadDBRead)
import           Pos.Txp              (MonadTxpMem, Utxo, addrBelongsToSet,
                                       getUtxoModifier)
import qualified Pos.Txp.DB           as DB
import           Pos.Txp.Toil.Utxo    (getTotalCoinsInUtxo)
import qualified Pos.Util.Modifier    as MM
import           Pos.Wallet.Web.State (WalletSnapshot)
import qualified Pos.Wallet.Web.State as WS

getOwnUtxos :: (MonadIO m, MonadDBRead m, MonadTxpMem ext ctx m)
            => WalletSnapshot -> [Address] -> m Utxo
getOwnUtxos ws addrs = do
    let (redeemAddrs, commonAddrs) = partition isRedeemAddress addrs

    updates <- getUtxoModifier
    let commonUtxo = if null commonAddrs then mempty
                     else WS.getWalletUtxo ws
    redeemUtxo <- if null redeemAddrs then pure mempty
                  else DB.getFilteredUtxo redeemAddrs

    let allUtxo = MM.modifyMap updates $ commonUtxo <> redeemUtxo
        addrsSet = HS.fromList addrs
    pure $ M.filter (`addrBelongsToSet` addrsSet) allUtxo

getBalanceFromUtxo :: Functor m => ([Address] -> m Utxo) -> Address -> m Coin
getBalanceFromUtxo getOwnUtxos' = fmap getTotalCoinsInUtxo . getOwnUtxos' . one

-- | Sometimes we want to get utxo for all addresses which we «own»,
-- i. e. can spend funds from them. We can't get all such addresses
-- from public key, because it's impossible to extract spending data
-- from an address. And we can't enumerate all possible addresses for
-- a public key. So we only consider two addresses: one with bootstrap
-- era distribution and another one with single key distribution.
getOwnUtxoForPk :: ([Address] -> m Utxo)
                -> PublicKey -> m Utxo
getOwnUtxoForPk getOwnUtxos' ourPk = getOwnUtxos' ourAddresses
  where
    ourAddresses :: [Address]
    ourAddresses =
        map (flip makePubKeyAddress ourPk . IsBootstrapEraAddr) [False, True]
