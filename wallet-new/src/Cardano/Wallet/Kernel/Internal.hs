{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel.Internal (
    -- * Passive wallet
    PassiveWallet(..)
  , ActiveWallet(..)
  , walletKeystore
  , walletMeta
  , wallets
  , walletLogMessage
  , walletNode
  ) where

import           Universum hiding (State)

import           Control.Lens.TH

import           System.Wlog (Severity (..))

import           Data.Acid (AcidState)

import           Cardano.Wallet.Kernel.DB.AcidState (DB)
import           Cardano.Wallet.Kernel.DB.TxMeta
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import           Cardano.Wallet.Kernel.Submission (WalletSubmission)

-- Handy re-export of the pure getters

import           Pos.Core (ProtocolMagic)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
data PassiveWallet = PassiveWallet {
      -- | Send log message
      _walletLogMessage :: Severity -> Text -> IO ()

      -- | Logger
    , _walletKeystore   :: Keystore

      -- | An opaque handle to a place where we store the 'EncryptedSecretKey'.
    , _wallets          :: AcidState DB

      -- | Database handle
    , _walletMeta       :: MetaDBHandle

      -- | Access to the underlying node
      --
      -- This should be used sparingly; for most purposes the wallet's own
      -- database should be consulted instead. For example, instead of asking
      -- the node "what is the current slot ID?" in most functions we should
      -- probably ask the wallet "what is the slot ID in the most recent
      -- checkpoint?". The wallet's state will track the node's but may be
      -- behind, and that's okay. The key is internal consistency.
      --
      -- The primary function of this is wallet restoration, where the wallet's
      -- own DB /cannot/ be consulted.
    , _walletNode       :: NodeStateAdaptor IO
    }

makeLenses ''PassiveWallet


{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive       :: PassiveWallet
      -- | The wallet diffusion layer
    , walletDiffusion     :: WalletDiffusion
      -- | The wallet submission layer
    , walletSubmission    :: MVar WalletSubmission
      -- | The protocol magic used to make transactions.
    , walletProtocolMagic :: ProtocolMagic
    }
