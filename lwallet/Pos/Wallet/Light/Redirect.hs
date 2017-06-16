{-# LANGUAGE TypeFamilies #-}

-- | Various monadic redirects implementing wallet constraints. Mostly stubs.

module Pos.Wallet.Light.Redirect
       ( getOwnUtxosWallet
       , getBalanceWallet
       , getTxHistoryWallet
       , saveTxWallet
       ) where

import           Universum

import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Data.Tagged                  (Tagged (..))
import qualified Ether

import           Pos.Client.Txp.Balances      (MonadBalances (..), getBalanceFromUtxo)
import           Pos.Client.Txp.History       (TxHistoryAnswer, deriveAddrHistory)
import           Pos.Core                     (Address, HeaderHash)
import           Pos.Txp                      (TxAux, TxId, Utxo, filterUtxoByAddrs,
                                               runUtxoStateT)
import           Pos.Types                    (Coin)
import qualified Pos.Wallet.Light.State       as LWS

----------------------------------------------------------------------------
-- MonadBalances
----------------------------------------------------------------------------

getOwnUtxosWallet :: (MonadIO m, Ether.MonadReader' LWS.WalletState m) => [Address] -> m Utxo
getOwnUtxosWallet addrs = filterUtxoByAddrs addrs <$> LWS.getUtxo

getBalanceWallet :: (MonadIO m, MonadBalances m) => Address -> m Coin
getBalanceWallet = getBalanceFromUtxo

----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

getTxHistoryWallet
    :: (Ether.MonadReader' LWS.WalletState m, MonadIO m)
    => Tagged ssc ([Address] -> Maybe (HeaderHash, Utxo) -> m TxHistoryAnswer)
getTxHistoryWallet = Tagged $ \addrs _ -> do
    chain <- LWS.getBestChain
    utxo <- LWS.getOldestUtxo
    _ <- fmap (fst . fromMaybe (error "deriveAddrHistory: Nothing")) $
        runMaybeT $ flip runUtxoStateT utxo $
        deriveAddrHistory addrs chain
    pure $ error "getTxHistory is not implemented for light wallet"

saveTxWallet :: Monad m => (TxId, TxAux) -> m ()
saveTxWallet _ = pure ()
