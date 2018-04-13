{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , bracketPassiveWallet
  , init
  , applyBlock
  , utxo
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  , hasPending
  , walletPassive
  ) where

import           System.Wlog (Severity (..))
import           Universum

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Types

import           Pos.Core (TxAux)
import           Pos.Txp (Utxo)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
-- TODO: This is just a placeholder for now, we'll want all kinds of state
-- in here.
data PassiveWallet = PassiveWallet {
      -- | Send log message
      walletLogMessage :: Severity -> Text -> IO ()
    }

-- | Allocate wallet resources
--
-- NOTE: See also 'init'.
--
-- TODO: Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: MonadMask m
                     => (Severity -> Text -> IO ())
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet walletLogMessage =
    bracket
      (return PassiveWallet{..})
      (\_ -> return ())

-- | Initialize the wallet
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> Utxo -> IO ()
init PassiveWallet{..} _utxo = do
    walletLogMessage Info "Wallet kernel initialized"

-- | Notify the wallet of a new block
applyBlock :: PassiveWallet -> ResolvedBlock -> IO ()
applyBlock _wallet _block = error "TODO: applyBlock"

-- | Return the wallet's current UTxO
utxo :: PassiveWallet -> IO Utxo
utxo _wallet = error "TODO: utxo"

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive   :: PassiveWallet

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWallet :: MonadMask m
                    => PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWallet{..})
      (\_ -> return ())

-- | Submit a new pending transaction
newPending :: ActiveWallet -> TxAux -> IO ()
newPending ActiveWallet{..} _tx = do
    walletLogMessage Error "TODO: Cardano.Wallet.Kernel.newPending"
  where
    PassiveWallet{..} = walletPassive

-- | Return True if there are pending transactions
hasPending :: ActiveWallet -> IO Bool
hasPending _ = return False
