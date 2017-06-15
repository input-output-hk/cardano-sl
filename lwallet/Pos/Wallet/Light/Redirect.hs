{-# LANGUAGE TypeFamilies #-}

-- | Various monadic redirects implementing wallet constraints. Mostly stubs.

module Pos.Wallet.Light.Redirect
       ( BalancesWalletRedirect
       , runBalancesWalletRedirect
       , getOwnUtxosWallet
       , getBalanceWallet
       , BlockchainInfoNotImplemented
       , runBlockchainInfoNotImplemented
       , TxHistoryWalletRedirect
       , runTxHistoryWalletRedirect
       , getTxHistoryWallet
       , saveTxWallet
       , UpdatesNotImplemented
       , runUpdatesNotImplemented
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Data.Coerce                  (coerce)
import           Data.Tagged                  (Tagged (..))
import qualified Ether

import           Pos.Client.Txp.Balances      (MonadBalances (..), getBalanceFromUtxo)
import           Pos.Client.Txp.History       (MonadTxHistory (..), TxHistoryAnswer,
                                               deriveAddrHistory)
import           Pos.Core                     (Address, HeaderHash)
import           Pos.Txp                      (TxAux, TxId, Utxo, filterUtxoByAddrs,
                                               runUtxoStateT)
import           Pos.Types                    (Coin)
import qualified Pos.Wallet.Light.State       as LWS
import           Pos.Wallet.WalletMode        (MonadBlockchainInfo (..),
                                               MonadUpdates (..))

----------------------------------------------------------------------------
-- MonadBalances
----------------------------------------------------------------------------

data BalancesWalletRedirectTag

type BalancesWalletRedirect =
    Ether.TaggedTrans BalancesWalletRedirectTag IdentityT

runBalancesWalletRedirect :: BalancesWalletRedirect m a -> m a
runBalancesWalletRedirect = coerce

getOwnUtxosWallet :: (MonadIO m, Ether.MonadReader' LWS.WalletState m) => [Address] -> m Utxo
getOwnUtxosWallet addrs = filterUtxoByAddrs addrs <$> LWS.getUtxo

getBalanceWallet :: (MonadIO m, MonadBalances m) => Address -> m Coin
getBalanceWallet = getBalanceFromUtxo

instance
    (MonadIO m, t ~ IdentityT, Ether.MonadReader' LWS.WalletState m) =>
        MonadBalances (Ether.TaggedTrans BalancesWalletRedirectTag t m)
  where
    getOwnUtxos = getOwnUtxosWallet
    getBalance = getBalanceWallet

----------------------------------------------------------------------------
-- MonadBlockchainInfo
----------------------------------------------------------------------------

-- | Stub instance for lite-wallet
data BlockchainInfoNotImplementedTag

type BlockchainInfoNotImplemented =
    Ether.TaggedTrans BlockchainInfoNotImplementedTag IdentityT

runBlockchainInfoNotImplemented :: BlockchainInfoNotImplemented m a -> m a
runBlockchainInfoNotImplemented = coerce

instance
    (t ~ IdentityT, Monad m) =>
        MonadBlockchainInfo (Ether.TaggedTrans BlockchainInfoNotImplementedTag t m)
  where
    networkChainDifficulty = error "notImplemented"
    localChainDifficulty = error "notImplemented"
    blockchainSlotDuration = error "notImplemented"
    connectedPeers = error "notImplemented"

----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

-- | Get tx history for Address
data TxHistoryWalletRedirectTag

type TxHistoryWalletRedirect =
    Ether.TaggedTrans TxHistoryWalletRedirectTag IdentityT

runTxHistoryWalletRedirect :: TxHistoryWalletRedirect m a -> m a
runTxHistoryWalletRedirect = coerce

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

instance
    ( MonadIO m
    , t ~ IdentityT
    , Ether.MonadReader' LWS.WalletState m
    ) => MonadTxHistory (Ether.TaggedTrans TxHistoryWalletRedirectTag t m)
  where
    getTxHistory = getTxHistoryWallet
    saveTx = saveTxWallet

----------------------------------------------------------------------------
-- MonadUpdates
----------------------------------------------------------------------------

-- | Dummy instance for lite-wallet
data UpdatesNotImplementedTag

type UpdatesNotImplemented =
    Ether.TaggedTrans UpdatesNotImplementedTag IdentityT

runUpdatesNotImplemented :: UpdatesNotImplemented m a -> m a
runUpdatesNotImplemented = coerce

instance
    ( t ~ IdentityT
    , MonadIO m
    ) => MonadUpdates (Ether.TaggedTrans UpdatesNotImplementedTag t m)
  where
    waitForUpdate = error "notImplemented"
    applyLastUpdate = pure ()
