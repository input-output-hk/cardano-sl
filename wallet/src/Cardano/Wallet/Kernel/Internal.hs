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
    -- ** Lenses
  , walletKeystore
  , walletMeta
  , wallets
  , walletProtocolMagic
  , walletLogMessage
  , walletNode
  , walletProtocolParams
  , walletSubmission
  , walletRestorationTask
  , walletFInjects
    -- * Active wallet
  , ActiveWallet(..)
    -- * Restoration data
  , WalletRestorationTask
  , WalletRestorationInfo(..)
  , WalletRestorationProgress(..)
    -- ** Utility functions
  , newRestorationTasks
  , addOrReplaceRestoration
  , removeRestoration
  , lookupRestorationInfo
  , currentRestorations
  , cancelRestoration
  , restartRestoration
  , stopAllRestorations
    -- ** Lenses
  , wrpCurrentSlot
  , wrpTargetSlot
  , wrpThroughput
  , wriProgress
  , wriCancel
  , wriRestart
  ) where

import           Universum hiding (State)

import qualified Control.Concurrent.MVar.Strict as Strict
import           Control.Lens (to)
import           Control.Lens.TH
import           Data.Acid (AcidState)
import qualified Data.Map.Strict as Map

import           Pos.Core (BlockCount, FlatSlotId)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.InjectFail (FInjects)
import           Pos.Util.Wlog (Severity (..))

import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..),
                     UnitOfMeasure (..))
import           Cardano.Wallet.Kernel.DB.AcidState (DB)
import           Cardano.Wallet.Kernel.DB.TxMeta
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import           Cardano.Wallet.Kernel.ProtocolParameters
import           Cardano.Wallet.Kernel.Submission (WalletSubmission)
import           Cardano.Wallet.Kernel.Types (WalletId)

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

      -- | The protocol magic used by an `ActiveWallet` to make transactions.
    , _walletProtocolMagic   :: ProtocolMagic

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

    , _walletProtocolParams  :: ProtocolParameterAdaptor

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
    , _walletSubmission      :: Strict.MVar WalletSubmission

      -- | Wallet restoration tasks. Wallets that are in the midst of a restoration
      -- will be doing background work to restore the history. This map holds a
      -- reference to the restoration background task, along with the current status
      -- of the task.
      --
      -- The invariant is that a WalletId should appear in this map if and only if
      -- that wallet is still undergoing restoration.
    , _walletRestorationTask :: WalletRestorationTask

      -- | Failure injection handle:  a stateful set of active fault injections.
    , _walletFInjects        :: FInjects IO
    }

{-------------------------------------------------------------------------------
  Restoration status
-------------------------------------------------------------------------------}

newtype WalletRestorationTask =
    WalletRestorationTask { _wrt :: Strict.MVar (Map WalletId WalletRestorationInfo) }

newRestorationTasks :: IO WalletRestorationTask
newRestorationTasks = WalletRestorationTask <$> newMVar Map.empty

-- | Wallet restoration information
--
-- The restoration info tracks the progress of a background wallet
-- restoration task currently in progress. In addition to giving
-- visibility into a restoration task, it also provides an action
-- that can be used to cancel the background restoration task.
data WalletRestorationInfo = WalletRestorationInfo
  { _wriProgress :: IO WalletRestorationProgress
     -- ^ Information on how the restoration is progressing.
  , _wriCancel   :: IO ()
    -- ^ The action that can be used to cancel the restoration task.
  , _wriRestart  :: IO ()
    -- ^ Restart the restoration task from scratch, using the current tip.
  }

-- Where is really nothing to force here.
instance NFData WalletRestorationInfo where
    rnf _ = ()

-- | Data needed to assess the progress of a wallet restoration.
data WalletRestorationProgress = WalletRestorationProgress
    { _wrpCurrentSlot :: FlatSlotId
      -- ^ The most recently restored slot
    , _wrpTargetSlot  :: FlatSlotId
      -- ^ The target slot; when restoration reaches this slot,
      -- it is finished and the wallet is up-to-date.
    , _wrpThroughput  :: MeasuredIn 'BlocksPerSecond BlockCount
      -- ^ Speed of restoration.
    }

makeLenses ''PassiveWallet
makeLenses ''WalletRestorationInfo
makeLenses ''WalletRestorationProgress

lookupRestorationInfo :: PassiveWallet -> WalletId -> IO (Maybe WalletRestorationInfo)
lookupRestorationInfo pw wid = Map.lookup wid <$> currentRestorations pw

addOrReplaceRestoration :: PassiveWallet -> WalletId -> WalletRestorationInfo -> IO ()
addOrReplaceRestoration pw wId restoreInfo =
    Strict.modifyMVar_ (pw ^. walletRestorationTask . to _wrt) $ \wrt -> do
        -- Cancel any other restorations currently running for this wallet.
       whenJust (Map.lookup wId wrt) cancelRestoration
       -- Register this restoration task with the wallet.
       return (Map.insert wId restoreInfo wrt)

removeRestoration :: PassiveWallet -> WalletId -> IO ()
removeRestoration pw wId = do
    wri <- lookupRestorationInfo pw wId
    Strict.modifyMVar_ (pw ^. walletRestorationTask . to _wrt) (pure . Map.delete wId)
    whenJust wri cancelRestoration

currentRestorations :: PassiveWallet -> IO (Map WalletId WalletRestorationInfo)
currentRestorations pw = readMVar (pw ^. walletRestorationTask . to _wrt)

cancelRestoration :: WalletRestorationInfo -> IO ()
cancelRestoration = _wriCancel

restartRestoration :: WalletRestorationInfo -> IO ()
restartRestoration = _wriRestart

stopAllRestorations :: PassiveWallet -> IO ()
stopAllRestorations pw = do
    Strict.modifyMVar_ (pw ^. walletRestorationTask . to _wrt) $ \mp -> do
        for_ mp cancelRestoration
        return Map.empty

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
