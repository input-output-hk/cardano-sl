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
  , WalletRestorationInfo(..)
    -- ** Lenses
  , walletKeystore
  , walletMeta
  , wallets
  , walletLogMessage
  , walletNode
  , walletSubmission
  , walletRestorationTask
  , wriCurrentSlot
  , wriTargetSlot
  , wriThroughput
  , wriCancel
    -- * Active wallet
  , ActiveWallet(..)
  ) where

import           Universum hiding (State)

import           Control.Lens.TH
import           Data.Acid (AcidState)

import           Pos.Core (BlockCount, FlatSlotId)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Util.Wlog (Severity (..))

import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..),
                     UnitOfMeasure (..))
import           Cardano.Wallet.Kernel.DB.AcidState (DB)
import           Cardano.Wallet.Kernel.DB.TxMeta
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import           Cardano.Wallet.Kernel.Submission (WalletSubmission)
import           Cardano.Wallet.Kernel.Types (WalletId)

{-------------------------------------------------------------------------------
  Restoration status
-------------------------------------------------------------------------------}

-- | Wallet restoration information
--
-- The restoration info tracks the progress of a background wallet
-- restoration task currently in progress. In addition to giving
-- visibility into a restoration task, it also provides an action
-- that can be used to cancel the background restoration task.
data WalletRestorationInfo = WalletRestorationInfo
  { _wriCurrentSlot :: FlatSlotId
     -- ^ The most recently restored slot
  , _wriTargetSlot  :: FlatSlotId
     -- ^ The target slot; when restoration reaches this slot,
     -- it is finished and the wallet is up-to-date.
  , _wriThroughput  :: MeasuredIn 'BlocksPerSecond BlockCount
    -- ^ Speed of restoration.
  , _wriCancel      :: IO Bool
    -- ^ The action that can be used to cancel the restoration task.
    -- This may fail, so we return a Bool.
  }

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
      _walletLogMessage      :: Severity -> Text -> IO ()

      -- | Logger
    , _walletKeystore        :: Keystore

      -- | An opaque handle to a place where we store the 'EncryptedSecretKey'.
    , _wallets               :: AcidState DB

      -- | Database handle
    , _walletMeta            :: MetaDBHandle

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
    , _walletNode            :: NodeStateAdaptor IO

      -- | The wallet submission layer
      --
      -- NOTE: Although the passive wallet cannot send transactions, it is
      -- important that the wallet submission layer itself lives in the
      -- passive wallet: in the 'BListener' interface, we need to be able to
      -- tell the submission layer when transactions got confirmed (when
      -- applying blocks) or need to reintroduced (due to rollback).
      --
      -- In a way, the submission layer needs the separate active/passive split:
      -- the passive part just registers and deregisters transactions, while
      -- the active part actually sends stuff across the network. Fortunately,
      -- we already have this split: the submission layer itself is just a
      -- pure data structure, and the sending happens in a separate thread.
    , _walletSubmission      :: MVar WalletSubmission

      -- | Wallet restoration tasks. Wallets that are in the midst of a restoration
      -- will be doing background work to restore the history. This map holds a
      -- reference to the restoration background task, along with the current status
      -- of the task.
      --
      -- The invariant is that a WalletId should appear in this map if and only if
      -- that wallet is still undergoing restoration.
    , _walletRestorationTask :: MVar (Map WalletId WalletRestorationInfo)
    }

makeLenses ''PassiveWallet
makeLenses ''WalletRestorationInfo

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
      -- | The protocol magic used to make transactions.
    , walletProtocolMagic :: ProtocolMagic
    }
