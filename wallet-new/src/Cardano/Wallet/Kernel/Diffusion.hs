{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.Kernel.Diffusion (
    WalletDiffusion(..)
  , fromDiffusion
  ) where

import           Universum

import           Pos.Core.Txp (TxAux)
import           Pos.Infra.Communication.Types.Protocol (NodeId)
import           Pos.Infra.Diffusion.Subscription.Status (SubscriptionStatus,
                     ssMap)
import           Pos.Infra.Diffusion.Types

-- | Wallet diffusion layer
--
-- Limited diffusion that the active wallet needs
--
-- This has two design objectives:
--
-- * We should be able to easily instantiate this from the unit tests
--   (with some kind of mock diffusion layer)
-- * We should be able to translate @Diffusion m@ into @WalletDiffusion@ for
--   any @m@ that we can lower to @IO@ (to isolate us from the specific monad
--   stacks that are used in the full node).
--
-- Note that the latter requirement implies avoiding functionality from the full
-- diffusion layer with negative occurrences of the monad parameter.
data WalletDiffusion = WalletDiffusion {
      -- | Submit a transaction to the network
      walletSendTx                :: TxAux -> IO Bool

      -- | Get subscription status (needed for the node settings endpoint)
    , walletGetSubscriptionStatus :: IO (Map NodeId SubscriptionStatus)
    }

-- | Extract necessary functionality from the full diffusion layer
fromDiffusion :: (forall a. m a -> IO a)
              -> Diffusion m
              -> WalletDiffusion
fromDiffusion nat d = WalletDiffusion {
      walletSendTx                = nat . sendTx d
    , walletGetSubscriptionStatus = readTVarIO $ ssMap (subscriptionStates d)
    }
