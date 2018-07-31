{-# LANGUAGE RankNTypes #-}
-- | Wallet state as mandated by the wallet specification
module Cardano.Wallet.Kernel.DB.Spec (
    -- * Checkpoint
    Checkpoint(..)
    -- ** Lenses
  , checkpointUtxo
  , checkpointUtxoBalance
  , checkpointPending
  , checkpointBlockMeta
  , checkpointChainBrief
    -- * Partial checkpoints
  , PartialCheckpoint(..)
  , fromFullCheckpoint
  , toFullCheckpoint
    -- ** Lenses
  , pcheckpointUtxo
  , pcheckpointUtxoBalance
  , pcheckpointPending
  , pcheckpointBlockMeta
  , pcheckpointChainBrief
    -- * Unify partial and full checkpoints
  , IsCheckpoint(..)
  , cpAddressMeta
    -- ** Convenience: lift lenses on single checkpoint to the most recent one
  , currentCheckpoint
  , currentUtxo
  , currentUtxoBalance
  , currentPending
  , currentBlockMeta
  , currentChainBrief
  , currentAddressMeta
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

import           Cardano.Wallet.Kernel.ChainState
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec.Pending
import           Cardano.Wallet.Kernel.Util (neHead)

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
    , _checkpointChainBrief  :: !ChainBrief
    }

makeLenses ''Checkpoint

deriveSafeCopy 1 'base ''Checkpoint

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
    , _pcheckpointChainBrief  :: !ChainBrief
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
        , _pcheckpointChainBrief  =        _checkpointChainBrief
        }

    inj :: PartialCheckpoint -> Checkpoint
    inj PartialCheckpoint{..} = Checkpoint{
          _checkpointUtxo        =        _pcheckpointUtxo
        , _checkpointUtxoBalance =        _pcheckpointUtxoBalance
        , _checkpointPending     =        _pcheckpointPending
        , _checkpointBlockMeta   = coerce _pcheckpointBlockMeta
        , _checkpointChainBrief  =        _pcheckpointChainBrief
        }

-- | Construct a full checkpoint from a partial checkpoint
--
-- We can do this only given the block metadata of the previous block. We
-- ask for the full previous checkpoint so that we can do a sanity check that
-- the checkpoints line up.
toFullCheckpoint :: Checkpoint -> PartialCheckpoint -> Checkpoint
toFullCheckpoint prev PartialCheckpoint{..} =
    if _pcheckpointChainBrief `chainBriefSucceeds` _checkpointChainBrief prev
      then Checkpoint {
               _checkpointUtxo        =          _pcheckpointUtxo
             , _checkpointUtxoBalance =          _pcheckpointUtxoBalance
             , _checkpointPending     =          _pcheckpointPending
             , _checkpointBlockMeta   = withPrev _pcheckpointBlockMeta
             , _checkpointChainBrief  =          _pcheckpointChainBrief
             }
      else error "toFullCheckpoint: checkpoints do not line up"
  where
    withPrev :: LocalBlockMeta -> BlockMeta
    withPrev = appendBlockMeta (_checkpointBlockMeta prev)

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
    cpChainBrief  :: Lens' c ChainBrief

instance IsCheckpoint Checkpoint where
    cpUtxo        = checkpointUtxo . fromDb
    cpUtxoBalance = checkpointUtxoBalance . fromDb
    cpPending     = checkpointPending
    cpBlockMeta   = checkpointBlockMeta . from _Wrapped
    cpChainBrief  = checkpointChainBrief

instance IsCheckpoint PartialCheckpoint where
    cpUtxo        = pcheckpointUtxo . fromDb
    cpUtxoBalance = pcheckpointUtxoBalance . fromDb
    cpPending     = pcheckpointPending
    cpBlockMeta   = pcheckpointBlockMeta
    cpChainBrief  = pcheckpointChainBrief

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
currentChainBrief  :: IsCheckpoint c =>                 Lens' (NewestFirst NonEmpty c) ChainBrief
currentAddressMeta :: IsCheckpoint c => Core.Address -> Lens' (NewestFirst NonEmpty c) AddressMeta

currentUtxo             = currentCheckpoint . cpUtxo
currentUtxoBalance      = currentCheckpoint . cpUtxoBalance
currentPending          = currentCheckpoint . cpPending
currentBlockMeta        = currentCheckpoint . cpBlockMeta
currentChainBrief       = currentCheckpoint . cpChainBrief
currentAddressMeta addr = currentCheckpoint . cpAddressMeta addr

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
