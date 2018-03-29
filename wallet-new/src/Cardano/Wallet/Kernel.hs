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
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  , hasPending
  ) where

import Universum
import System.Wlog (Severity(..))

import Cardano.Wallet.Kernel.Diffusion (WalletDiffusion(..))
import Cardano.Wallet.WalletLayer (PassiveWalletLayer)

import Pos.Core (TxAux)

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
data PassiveWallet m = PassiveWallet {
      -- | The data layer for the wallet calling the database.
      passiveWalletLayer  :: PassiveWalletLayer m

      -- | Send log message
    , walletLogMessage    :: Severity -> Text -> IO ()
    }

-- | Allocate wallet resources
--
-- NOTE: See also 'init'.
--
-- TODO: Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: forall m n a. (MonadMask m, Monad n)
                     => (Severity -> Text -> IO ())
                     -> PassiveWalletLayer n
                     -> (PassiveWallet n -> m a) -> m a
bracketPassiveWallet walletLogMessage passiveWalletLayer =
    bracket
      (return PassiveWallet{..})
      (\_ -> return ())

-- | Initialize the wallet
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: forall m. PassiveWallet m -> IO ()
init PassiveWallet{..} = do
    walletLogMessage Info "Wallet kernel initialized"

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet m = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive   :: PassiveWallet m

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWallet :: forall m n a. (MonadMask m, Monad n)
                    => PassiveWallet n
                    -> WalletDiffusion
                    -> (ActiveWallet n -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWallet{..})
      (\_ -> return ())

-- | Submit a new pending transaction
newPending :: forall m. ActiveWallet m -> TxAux -> IO ()
newPending ActiveWallet{..} _tx = do
    walletLogMessage Error "TODO: Cardano.Wallet.Kernel.newPending"
  where
    PassiveWallet{..} = walletPassive

-- | Return True if there are pending transactions
hasPending :: forall m. ActiveWallet m -> IO Bool
hasPending _ = return False

