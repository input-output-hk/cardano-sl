{-# LANGUAGE RankNTypes #-}
-- | Wallet state as mandated by the wallet specification
module Cardano.Wallet.Kernel.DB.Spec (
    -- * Checkpoint
    Checkpoint(..)
  , initCheckpoint
    -- ** Lenses
  , checkpointUtxo
  , checkpointUtxoBalance
  , checkpointPending
  , checkpointBlockMeta
  , checkpointSlotId
  , checkpointForeign
    -- * Partial checkpoints
  , PartialCheckpoint(..)
  , fromFullCheckpoint
  , toFullCheckpoint
    -- ** Lenses
  , pcheckpointUtxo
  , pcheckpointUtxoBalance
  , pcheckpointPending
  , pcheckpointBlockMeta
  , pcheckpointSlotId
  , pcheckpointForeign
    -- * Unify partial and full checkpoints
  , IsCheckpoint(..)
  , cpAddressMeta
    -- ** Convenience: lift lenses on single checkpoint to the most recent one
  , currentCheckpoint
  , currentUtxo
  , currentUtxoBalance
  , currentPending
  , currentBlockMeta
  , currentSlotId
  , currentAddressMeta
  , currentForeign
  ) where

import           Universum

import           Control.Lens (from, _Wrapped)
import           Control.Lens.TH (makeLenses)
import           Data.Coerce (coerce)
import           Data.SafeCopy (base, deriveSafeCopy)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util.Text (mapJson)

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import           Pos.Core.Chrono (NewestFirst)

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.Util (neHead)
import           Cardano.Wallet.Kernel.Util.Core as Core

{-------------------------------------------------------------------------------
  Wallet state as mandated by the spec
-------------------------------------------------------------------------------}

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
      _checkpointUtxo        :: !(InDb Core.Utxo)
    , _checkpointUtxoBalance :: !(InDb Core.Coin)
    , _checkpointPending     :: !Pending
    , _checkpointBlockMeta   :: !BlockMeta

     -- | Slot ID associated with this checkpoint
     --
     -- This is used for restoration to know (1) when we bridged the gap between
     -- the partial current checkpoints and the full historical checkpoints and
     -- (2) to be able to report how synchronization progress
    , _checkpointSlotId      :: !(InDb Core.SlotId)

      -- Foreign pending transactions are transactions that transfer funds from
      -- /other/ wallets /to/ this wallet. An example are redemption
      -- certificates, which (logically) transfer money from an "AVVM wallet" to
      -- this one; crucially, this wallet would not recognize the input of a
      -- redemption transaction as " ours ".
    , _checkpointForeign     :: !Pending
    }

makeLenses ''Checkpoint

deriveSafeCopy 1 'base ''Checkpoint

-- | Initial checkpoint for an account
--
-- This takes a UTxO as argument to allow for wallets that are given an initial
-- UTxO in the genesis block (note that we never roll back past the initial
-- checkpoint).
--
-- The slot ID for all initial checkpoints is always set to slot 0 of epoch 0.
-- One way to think about this is that semantically we regard all accounts to
-- be created at the beginning of time.
initCheckpoint :: Core.Utxo -> Checkpoint
initCheckpoint utxo = Checkpoint {
      _checkpointUtxo        = InDb utxo
    , _checkpointUtxoBalance = InDb $ Core.utxoBalance utxo
    , _checkpointPending     = Pending.empty
    , _checkpointForeign     = Pending.empty
    , _checkpointBlockMeta   = emptyBlockMeta
    , _checkpointSlotId      = InDb $ Core.SlotId
                                 (Core.EpochIndex 0)
                                 (Core.UnsafeLocalSlotIndex 0)
    }

{-------------------------------------------------------------------------------
  Partial checkpoints
-------------------------------------------------------------------------------}

-- | Partial checkpoint
--
-- Partial checkpoints arise during wallet restoration. They differ from
-- regular checkpoints only in that we lack historical context and therefore
-- cannot construct complete block metadata. Instead, we must approximate the
-- block metadata using local information only. The intention is that if there
-- are multiple partial checkpoints, we accumulate local block metadata so that
-- the most accurate information possible.
data PartialCheckpoint = PartialCheckpoint {
      _pcheckpointUtxo        :: !(InDb Core.Utxo)
    , _pcheckpointUtxoBalance :: !(InDb Core.Coin)
    , _pcheckpointPending     :: !Pending
    , _pcheckpointBlockMeta   :: !LocalBlockMeta
    , _pcheckpointSlotId      :: !(InDb Core.SlotId)
    , _pcheckpointForeign     :: !Pending
    }

makeLenses ''PartialCheckpoint

deriveSafeCopy 1 'base ''PartialCheckpoint

-- | A full check point can be " downcast " to a partial checkpoint by
-- forgetting that we have complete block metadata.
--
-- We /could/ define this as an 'Iso', but prefer to define it as a 'Lens'
-- to emphasize that a partial checkpoint records less information than a
-- full checkpoint.
fromFullCheckpoint :: Lens' Checkpoint PartialCheckpoint
fromFullCheckpoint f cp = inj <$> f (proj cp)
  where
    proj :: Checkpoint -> PartialCheckpoint
    proj Checkpoint{..} = PartialCheckpoint {
          _pcheckpointUtxo        =        _checkpointUtxo
        , _pcheckpointUtxoBalance =        _checkpointUtxoBalance
        , _pcheckpointPending     =        _checkpointPending
        , _pcheckpointBlockMeta   = coerce _checkpointBlockMeta
        , _pcheckpointSlotId      =        _checkpointSlotId
        , _pcheckpointForeign     =        _checkpointForeign
        }

    inj :: PartialCheckpoint -> Checkpoint
    inj PartialCheckpoint{..} = Checkpoint{
          _checkpointUtxo        =        _pcheckpointUtxo
        , _checkpointUtxoBalance =        _pcheckpointUtxoBalance
        , _checkpointPending     =        _pcheckpointPending
        , _checkpointBlockMeta   = coerce _pcheckpointBlockMeta
        , _checkpointSlotId      =        _pcheckpointSlotId
        , _checkpointForeign     =        _pcheckpointForeign
        }

-- | Construct a full checkpoint from a partial checkpoint
--
-- We can do this only given the block metadata of the previous block. We
-- ask for the full previous checkpoint so that we can do a sanity check that
-- the checkpoints line up.
toFullCheckpoint :: Checkpoint -> PartialCheckpoint -> Checkpoint
toFullCheckpoint prev PartialCheckpoint{..} =
    if _pcheckpointSlotId `succeeds` _checkpointSlotId prev
      then Checkpoint {
               _checkpointUtxo        =          _pcheckpointUtxo
             , _checkpointUtxoBalance =          _pcheckpointUtxoBalance
             , _checkpointPending     =          _pcheckpointPending
             , _checkpointBlockMeta   = withPrev _pcheckpointBlockMeta
             , _checkpointSlotId      =          _pcheckpointSlotId
             , _checkpointForeign     =          _pcheckpointForeign
             }
      else error "toFullCheckpoint: checkpoints do not line up"
  where
    withPrev :: LocalBlockMeta -> BlockMeta
    withPrev = appendBlockMeta (_checkpointBlockMeta prev)

    -- We cannot check whether this is the _direct_ successor, since
    --
    -- 1. We don't know how many blocks in an epoch
    --    (though we could conceivably pass that in as an argument)
    -- 2. We may be skipping an EBB
    succeeds :: InDb Core.SlotId -> InDb Core.SlotId -> Bool
    InDb a `succeeds` InDb b = or [
          Core.siEpoch a > Core.siEpoch b
        , and [ Core.siEpoch a == Core.siEpoch b
              , Core.siSlot  a >  Core.siSlot  b
              ]
        ]

{-------------------------------------------------------------------------------
  Unify over full and partial checkpoints
-------------------------------------------------------------------------------}

-- | Unify over full and partial checkpoints
--
-- Although we can treat a full checkpoint as a partial checkpoint (through
-- 'fromFullCheckpoint'), writing
--
-- > foo :: PartialCheckpoint -> ...
--
-- suggests that @foo@ should only be applied to partial checkpoints; it's
-- cleaner to write
--
-- > foo :: IsCheckpoint c => c -> ...
--
-- for functions @foo@ that can be applied to either
class IsCheckpoint c where
    cpUtxo        :: Lens' c Core.Utxo
    cpUtxoBalance :: Lens' c Core.Coin
    cpPending     :: Lens' c Pending
    cpBlockMeta   :: Lens' c LocalBlockMeta
    cpSlotId      :: Lens' c Core.SlotId
    cpForeign     :: Lens' c Pending

instance IsCheckpoint Checkpoint where
    cpUtxo        = checkpointUtxo . fromDb
    cpUtxoBalance = checkpointUtxoBalance . fromDb
    cpPending     = checkpointPending
    cpBlockMeta   = checkpointBlockMeta . from _Wrapped
    cpSlotId      = checkpointSlotId . fromDb
    cpForeign     = checkpointForeign

instance IsCheckpoint PartialCheckpoint where
    cpUtxo        = pcheckpointUtxo . fromDb
    cpUtxoBalance = pcheckpointUtxoBalance . fromDb
    cpPending     = pcheckpointPending
    cpBlockMeta   = pcheckpointBlockMeta
    cpSlotId      = pcheckpointSlotId . fromDb
    cpForeign     = pcheckpointForeign

cpAddressMeta :: IsCheckpoint c => Core.Address -> Lens' c AddressMeta
cpAddressMeta addr = cpBlockMeta . _Wrapped . addressMeta addr

{-------------------------------------------------------------------------------
  Convenience: lift lenses on single checkpoint to the most recent one
-------------------------------------------------------------------------------}

currentCheckpoint :: Lens' (NewestFirst NonEmpty c) c
currentCheckpoint = _Wrapped . neHead

currentUtxo        :: IsCheckpoint c =>                 Lens' (NewestFirst NonEmpty c) Core.Utxo
currentUtxoBalance :: IsCheckpoint c =>                 Lens' (NewestFirst NonEmpty c) Core.Coin
currentPending     :: IsCheckpoint c =>                 Lens' (NewestFirst NonEmpty c) Pending
currentBlockMeta   :: IsCheckpoint c =>                 Lens' (NewestFirst NonEmpty c) LocalBlockMeta
currentSlotId      :: IsCheckpoint c =>                 Lens' (NewestFirst NonEmpty c) Core.SlotId
currentAddressMeta :: IsCheckpoint c => Core.Address -> Lens' (NewestFirst NonEmpty c) AddressMeta
currentForeign     :: IsCheckpoint c =>                 Lens' (NewestFirst NonEmpty c) Pending

currentUtxo             = currentCheckpoint . cpUtxo
currentUtxoBalance      = currentCheckpoint . cpUtxoBalance
currentPending          = currentCheckpoint . cpPending
currentBlockMeta        = currentCheckpoint . cpBlockMeta
currentSlotId           = currentCheckpoint . cpSlotId
currentAddressMeta addr = currentCheckpoint . cpAddressMeta addr
currentForeign          = currentCheckpoint . cpForeign

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

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
