-- | Wallet state as mandated by the wallet specification
module Cardano.Wallet.Kernel.DB.Spec (
    -- * Wallet state as mandated by the spec
    Pending(..)
  , Checkpoint(..)
  , Checkpoints
  , emptyPending
    -- ** Lenses
  , pendingTransactions
  , checkpointUtxo
  , checkpointUtxoBalance
  , checkpointExpected
  , checkpointPending
  , checkpointBlockMeta
    -- ** Lenses into the current checkpoint
  , currentCheckpoint
  , currentUtxo
  , currentUtxoBalance
  , currentExpected
  , currentPending
  , currentBlockMeta
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Data.SafeCopy (base, deriveSafeCopy)

import qualified Pos.Core as Core
import qualified Pos.Txp as Core

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb

{-------------------------------------------------------------------------------
  Wallet state as mandated by the spec
-------------------------------------------------------------------------------}

-- | Pending transactions
data Pending = Pending {
      _pendingTransactions :: InDb (Map Core.TxId Core.TxAux)
    }

-- | Returns a new, empty 'Pending' set.
emptyPending :: Pending
emptyPending = Pending . InDb $ mempty

-- | Per-wallet state
--
-- This is the same across all wallet types.
data Checkpoint = Checkpoint {
      _checkpointUtxo        :: InDb Core.Utxo
    , _checkpointUtxoBalance :: InDb Core.Coin
    , _checkpointExpected    :: InDb Core.Utxo
    , _checkpointPending     :: Pending
    , _checkpointBlockMeta   :: BlockMeta
    }

-- | List of checkpoints
type Checkpoints = NonEmpty Checkpoint

makeLenses ''Pending
makeLenses ''Checkpoint

deriveSafeCopy 1 'base ''Pending
deriveSafeCopy 1 'base ''Checkpoint

{-------------------------------------------------------------------------------
  Lenses for accessing current checkpoint
-------------------------------------------------------------------------------}

currentCheckpoint :: Lens' Checkpoints Checkpoint
currentCheckpoint = neHead

currentUtxo        :: Lens' Checkpoints Core.Utxo
currentUtxoBalance :: Lens' Checkpoints Core.Coin
currentExpected    :: Lens' Checkpoints Core.Utxo
currentPending     :: Lens' Checkpoints Pending
currentBlockMeta   :: Lens' Checkpoints BlockMeta

currentUtxo        = currentCheckpoint . checkpointUtxo        . fromDb
currentUtxoBalance = currentCheckpoint . checkpointUtxoBalance . fromDb
currentExpected    = currentCheckpoint . checkpointExpected    . fromDb
currentPending     = currentCheckpoint . checkpointPending
currentBlockMeta   = currentCheckpoint . checkpointBlockMeta

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

neHead :: Lens' (NonEmpty a) a
neHead f (x :| xs) = (:| xs) <$> f x
