{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.Balances
       ( MonadBalances(..)
       , getOwnUtxo
       , getBalanceFromUtxo
       , getOwnUtxosGenesis
       , getOwnUtxoForPk
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)

import           Pos.Core (Address (..), Coin, HasConfiguration, IsBootstrapEraAddr (..),
                           makePubKeyAddress)
import           Pos.Crypto (PublicKey)
import           Pos.Txp (Utxo, filterUtxoByAddrs, genesisUtxo, unGenesisUtxo)
import           Pos.Txp.Toil.Utxo (getTotalCoinsInUtxo)

-- | A class which have the methods to get state of address' balance
class Monad m => MonadBalances m where
    getOwnUtxos :: [Address] -> m Utxo
    getBalance :: Address -> m Coin
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

instance {-# OVERLAPPABLE #-}
    (MonadBalances m, MonadTrans t, Monad (t m)) =>
        MonadBalances (t m)
  where
    getOwnUtxos = lift . getOwnUtxos
    getBalance = lift . getBalance

getBalanceFromUtxo :: MonadBalances m => Address -> m Coin
getBalanceFromUtxo addr = getTotalCoinsInUtxo <$> getOwnUtxo addr

getOwnUtxosGenesis :: (HasConfiguration, Applicative m) => [Address] -> m Utxo
getOwnUtxosGenesis addrs = pure $ filterUtxoByAddrs addrs (unGenesisUtxo genesisUtxo)

getOwnUtxo :: MonadBalances m => Address -> m Utxo
getOwnUtxo = getOwnUtxos . one

-- | Sometimes we want to get utxo for all addresses which we «own»,
-- i. e. can spend funds from them. We can't get all such addresses
-- from public key, because it's impossible to extract spending data
-- from an address. And we can't enumerate all possible addresses for
-- a public key. So we only consider two addresses: one with bootstrap
-- era distribution and another one with single key distribution.
getOwnUtxoForPk :: MonadBalances m => PublicKey -> m Utxo
getOwnUtxoForPk ourPk = getOwnUtxos ourAddresses
  where
    ourAddresses :: [Address]
    ourAddresses =
        map (flip makePubKeyAddress ourPk . IsBootstrapEraAddr) [False, True]
