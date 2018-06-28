{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.Kernel.Diffusion (
    WalletDiffusion(..)
  , fromDiffusion
  ) where

import           Universum

import           Pos.Core
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
--
-- TODO: Right now this just provides means to send transactions; we might
-- also need to add some other functions (like delegation or voting).
data WalletDiffusion = WalletDiffusion {
      walletSendTx :: TxAux -> IO Bool
    }

-- | Extract necessary functionality from the full diffusion layer
fromDiffusion :: (forall a. m a -> IO a)
              -> Diffusion m
              -> WalletDiffusion
fromDiffusion nat d = WalletDiffusion {
      walletSendTx = nat . sendTx d
    }
