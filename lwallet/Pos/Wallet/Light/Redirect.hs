{-# LANGUAGE TypeFamilies #-}

-- | Various monadic redirects implementing wallet constraints. Mostly stubs.

module Pos.Wallet.Light.Redirect
       ( getOwnUtxosWallet
       , getBalanceWallet
       , getBlockHistoryWallet
       , getLocalHistoryWallet
       , saveTxWallet
       ) where

import           Universum

import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.DList                (DList)
import           Ether.Internal            (HasLens (..))

import           Pos.Client.Txp.Balances   (MonadBalances (..), getBalanceFromUtxo)
import           Pos.Client.Txp.History    (TxHistoryEntry, deriveAddrHistory)
import           Pos.Core                  (Address)
import           Pos.Txp                   (TxAux, TxId, Utxo, filterUtxoByAddrs,
                                            runUtxoStateT)
import           Pos.Types                 (Coin)
import qualified Pos.Wallet.Light.State    as LWS

----------------------------------------------------------------------------
-- MonadBalances
----------------------------------------------------------------------------

getOwnUtxosWallet :: (MonadIO m, MonadReader ctx m, HasLens LWS.WalletState ctx LWS.WalletState) => [Address] -> m Utxo
getOwnUtxosWallet addrs = filterUtxoByAddrs addrs <$> LWS.getUtxo

getBalanceWallet :: (MonadBalances m) => Address -> m Coin
getBalanceWallet = getBalanceFromUtxo

----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

getBlockHistoryWallet
    :: (MonadReader ctx m, HasLens LWS.WalletState ctx LWS.WalletState, MonadIO m)
    => [Address] -> m (DList TxHistoryEntry)
getBlockHistoryWallet addrs = do
    chain <- LWS.getBestChain
    utxo <- LWS.getOldestUtxo
    _ <- fmap (fst . fromMaybe (error "deriveAddrHistory: Nothing")) $
        runMaybeT $ flip runUtxoStateT utxo $
        deriveAddrHistory addrs chain
    pure $ error "getBlockHistory is not implemented for light wallet"

getLocalHistoryWallet
    :: [Address] -> m (DList TxHistoryEntry)
getLocalHistoryWallet = pure $
    error "getLocalHistory is not implemented for light wallet"

saveTxWallet :: Monad m => (TxId, TxAux) -> m ()
saveTxWallet _ = pure ()
