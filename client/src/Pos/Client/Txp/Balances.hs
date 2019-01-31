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

import           Pos.Chain.Genesis (GenesisData (..),
                     GenesisProtocolConstants (..))
import           Pos.Chain.Txp (Utxo, filterUtxoByAddrs, genesisUtxo,
                     getTotalCoinsInUtxo)
import           Pos.Core (Address (..), Coin, IsBootstrapEraAddr (..),
                     makePubKeyAddress)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (PublicKey)

-- | A class which have the methods to get state of address' balance
class Monad m => MonadBalances m where
    getOwnUtxos :: GenesisData -> [Address] -> m Utxo
    getBalance :: GenesisData -> Address -> m Coin
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

instance {-# OVERLAPPABLE #-}
    (MonadBalances m, MonadTrans t, Monad (t m)) =>
        MonadBalances (t m)
  where
    getOwnUtxos genesisData = lift . getOwnUtxos genesisData
    getBalance genesisData = lift . getBalance genesisData

getBalanceFromUtxo :: MonadBalances m => GenesisData -> Address -> m Coin
getBalanceFromUtxo genesisData addr =
    getTotalCoinsInUtxo <$> getOwnUtxo genesisData addr

{-# INLINE getOwnUtxosGenesis #-}
getOwnUtxosGenesis :: Applicative m => GenesisData -> [Address] -> m Utxo
getOwnUtxosGenesis genesisData addrs =
    pure $ filterUtxoByAddrs addrs $ genesisUtxo genesisData

getOwnUtxo :: MonadBalances m => GenesisData -> Address -> m Utxo
getOwnUtxo genesisData = getOwnUtxos genesisData . one

-- | Sometimes we want to get utxo for all addresses which we «own»,
-- i. e. can spend funds from them. We can't get all such addresses
-- from public key, because it's impossible to extract spending data
-- from an address. And we can't enumerate all possible addresses for
-- a public key. So we only consider two addresses: one with bootstrap
-- era distribution and another one with single key distribution.
getOwnUtxoForPk :: MonadBalances m => GenesisData -> PublicKey -> m Utxo
getOwnUtxoForPk genesisData ourPk = getOwnUtxos genesisData ourAddresses
  where
    nm :: NetworkMagic
    nm = makeNetworkMagic . gpcProtocolMagic . gdProtocolConsts $ genesisData
    --
    ourAddresses :: [Address]
    ourAddresses =
        map (flip (makePubKeyAddress nm) ourPk . IsBootstrapEraAddr) [False, True]
