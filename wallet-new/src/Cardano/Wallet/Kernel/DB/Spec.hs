-- | Wallet state as mandated by the wallet specification
module Cardano.Wallet.Kernel.DB.Spec (
    -- * Wallet state as mandated by the spec
    Pending(..)
  , PendingTxs
  , Balance
  , Checkpoint(..)
  , Checkpoints
  , emptyPending
  , singletonPending
  , unionPending
  , removePending
    -- ** Lenses
  , pendingTransactions
  , checkpointUtxo
  , checkpointUtxoBalance
  , checkpointPending
  , checkpointBlockMeta
    -- ** Lenses into the current checkpoint
  , currentCheckpoint
  , currentUtxo
  , currentUtxoBalance
  , currentPending
  , currentPendingTxs
  , currentBlockMeta
  ) where

import           Universum hiding (elems)

import           Control.Lens (to)
import           Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as M
import           Data.SafeCopy (base, deriveSafeCopy)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util.Text (listJsonIndent, mapJson)

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb

{-------------------------------------------------------------------------------
  Wallet state as mandated by the spec
-------------------------------------------------------------------------------}

type Balance = Integer

type PendingTxs = Map Txp.TxId Txp.TxAux

-- | Pending transactions
data Pending = Pending {
      _pendingTransactions :: InDb PendingTxs
     } deriving Eq


-- | Returns a new, empty 'Pending' set.
emptyPending :: Pending
emptyPending = Pending . InDb $ mempty

-- | Returns a new, empty 'Pending' set.
singletonPending :: Txp.TxId -> Txp.TxAux -> Pending
singletonPending txId txAux = Pending . InDb $ M.singleton txId txAux

-- | Computes the union between two 'Pending' sets.
unionPending :: Pending -> Pending -> Pending
unionPending (Pending new) (Pending old) =
    Pending (M.union <$> new <*> old)

-- | Computes the difference between two 'Pending' sets.
removePending :: Set Txp.TxId -> Pending -> Pending
removePending ids (Pending (InDb old)) = Pending (InDb $ old `withoutKeys` ids)
    where
        withoutKeys :: Ord k => Map k a -> Set k -> Map k a
        m `withoutKeys` s = m `M.difference` M.fromSet (const ()) s

-- | Per-wallet state
--
-- NOTE: At the moment this does not included the expected UTxO. The expected
-- UTxO is used for two things:
--
-- * Block resolution (translating tx inputs to their corresponding outputs, so
--   that we know the corresponding addresses, needed for prefilering)
-- * Minimum balance computation
--
-- Fortunately however we can rely on a full node as backing, so we don't need
-- to use the expected UTxO for block resolution (this is explained in the
-- formal spec in section "Prefiltering -- Consequences", under "possible
-- alternatives"), and minimum balance computation is a new feature that we
-- haven't implemented yet.
--
-- NOTE: This is the same across all wallet types.
data Checkpoint = Checkpoint {
      _checkpointUtxo        :: InDb Core.Utxo
    , _checkpointUtxoBalance :: InDb Core.Coin
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
currentBlockMeta   :: Lens' Checkpoints BlockMeta
currentPending     :: Lens' Checkpoints Pending
currentPendingTxs  :: Lens' Checkpoints PendingTxs

currentUtxo        = currentCheckpoint . checkpointUtxo        . fromDb
currentUtxoBalance = currentCheckpoint . checkpointUtxoBalance . fromDb
currentBlockMeta   = currentCheckpoint . checkpointBlockMeta
currentPending     = currentCheckpoint . checkpointPending
currentPendingTxs  = currentPending . pendingTransactions . fromDb

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

neHead :: Lens' (NonEmpty a) a
neHead f (x :| xs) = (:| xs) <$> f x

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Pending where
    build (Pending p) =
      let elems = p ^. fromDb . to M.toList
      in bprint ("Pending " % listJsonIndent 4) (map fst elems)

instance Buildable Checkpoint where
    build Checkpoint{..} = bprint
        ( "Checkpoint"
        % "{ utxo:        " % mapJson
        % ", utxoBalance: " % build
        % ", pending:     " % build
        % ", blockMeta:   " % build
        % "}"
        )
      (_fromDb _checkpointUtxo)
      (_fromDb _checkpointUtxoBalance)
      _checkpointPending
      _checkpointBlockMeta
