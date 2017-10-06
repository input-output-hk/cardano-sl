{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.Balances
       ( MonadBalances(..)
       , getOwnUtxo
       , getBalanceFromUtxo
       , getOwnUtxosDefault
       , getBalanceDefault
       , getOwnUtxoForPk
       ) where

import           Universum

import           Control.Monad.Trans  (MonadTrans)
import qualified Data.HashSet         as HS
import           Data.List            (partition)
import qualified Data.Map             as M

import           Pos.Core             (Address (..), Coin, IsBootstrapEraAddr (..),
                                       isRedeemAddress, makePubKeyAddress)
import           Pos.Crypto           (PublicKey)
import           Pos.DB               (MonadDBRead, MonadGState, MonadRealDB)
import           Pos.Txp              (MonadTxpMem, Utxo, addrBelongsToSet,
                                       getUtxoModifier)
import qualified Pos.Txp.DB           as DB
import           Pos.Txp.Toil.Utxo    (getTotalCoinsInUtxo)
import qualified Pos.Util.Modifier    as MM
import           Pos.Wallet.Web.State (WebWalletModeDB)
import qualified Pos.Wallet.Web.State as WS

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

type BalancesEnv ext ctx m =
    ( MonadRealDB ctx m
    , MonadDBRead m
    , MonadGState m
    , WebWalletModeDB ctx m
    , MonadMask m
    , MonadTxpMem ext ctx m)

getOwnUtxosDefault :: BalancesEnv ext ctx m => [Address] -> m Utxo
getOwnUtxosDefault addrs = do
    let (redeemAddrs, commonAddrs) = partition isRedeemAddress addrs

    updates <- getUtxoModifier
    commonUtxo <- if null commonAddrs then pure mempty
                  else WS.getWalletUtxo
    redeemUtxo <- if null redeemAddrs then pure mempty
                  else DB.getFilteredUtxo redeemAddrs

    let allUtxo = MM.modifyMap updates $ commonUtxo <> redeemUtxo
        addrsSet = HS.fromList addrs
    pure $ M.filter (`addrBelongsToSet` addrsSet) allUtxo

-- | `BalanceDB` isn't used here anymore, because
-- 1) It doesn't represent actual balances of addresses, but it represents _stakes_
-- 2) Local utxo is now cached, and deriving balances from it is not
--    so bad for performance now
getBalanceDefault :: (MonadBalances m) => Address -> m Coin
getBalanceDefault addr = getBalanceFromUtxo addr

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
