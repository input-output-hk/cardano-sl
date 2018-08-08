-- | Resolved blocks and transactions
module Cardano.Wallet.Kernel.DB.Resolved (
    -- * Resolved blocks and transactions
    ResolvedInput
  , ResolvedTx(..)
  , ResolvedBlock(..)
    -- ** Lenses
  , rtxInputs
  , rtxOutputs
  , rbTxs
  , rbBrief
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import qualified Data.Map as Map
-- import           Data.SafeCopy (base, deriveSafeCopy)
import           Formatting (bprint, (%))
import           Formatting.Buildable

import           Serokell.Util (listJson, mapJson)

import qualified Pos.Chain.Txp as Core

import           Cardano.Wallet.Kernel.ChainState
import           Cardano.Wallet.Kernel.DB.InDb

{-------------------------------------------------------------------------------
  Resolved blocks and transactions
-------------------------------------------------------------------------------}

-- | Resolved input
--
-- A transaction input @(h, i)@ points to the @i@th output of the transaction
-- with hash @h@, which is not particularly informative. The corresponding
-- 'ResolvedInput' is obtained by looking up what that output actually is.
type ResolvedInput = Core.TxOutAux

-- | (Unsigned) transaction with inputs resolved
--
-- NOTE: We cannot recover the original transaction from a 'ResolvedTx'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedTx = ResolvedTx {
      -- | Transaction inputs
      _rtxInputs  :: InDb (NonEmpty (Core.TxIn, ResolvedInput))

      -- | Transaction outputs
    , _rtxOutputs :: InDb Core.Utxo
    }

-- | (Unsigned block) containing resolved transactions
--
-- NOTE: We cannot recover the original block from a 'ResolvedBlock'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedBlock = ResolvedBlock {
      -- | Transactions in the block
      _rbTxs   :: ![ResolvedTx]

      -- | The chain state after applying this block
      --
      -- NOTE: The tip in the chain brief should correspond to the block we
      -- just applied.
      --
      -- TODO: Ideally we'd have an assertion somehow checking that.
    , _rbBrief :: !ChainBrief
    }

makeLenses ''ResolvedTx
makeLenses ''ResolvedBlock

-- TODO: Why are these necessary? and if they're not, get rid of InDb
--deriveSafeCopy 1 'base ''ResolvedTx
--deriveSafeCopy 1 'base ''ResolvedBlock

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable ResolvedTx where
  build ResolvedTx{..} = bprint
    ( "ResolvedTx "
    % "{ inputs:  " % mapJson
    % ", outputs: " % mapJson
    % "}"
    )
    (Map.fromList (toList (_rtxInputs  ^. fromDb)))
    (_rtxOutputs ^. fromDb)

instance Buildable ResolvedBlock where
  build ResolvedBlock{..} = bprint
    ( "ResolvedBlock "
    % "{ txs: " % listJson
    % "}"
    )
    _rbTxs
