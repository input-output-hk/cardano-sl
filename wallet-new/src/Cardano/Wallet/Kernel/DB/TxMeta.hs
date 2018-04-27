-- | Transaction metadata conform the wallet specification
module Cardano.Wallet.Kernel.DB.TxMeta (
    -- * Transaction metadata
    TxMeta(..)
    -- ** Lenses
  , txMetaId
  , txMetaAmount
  , txMetaInputs
  , txMetaOutputs
  , txMetaCreationAt
  , txMetaIsLocal
  , txMetaIsOutgoing
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)

import qualified Pos.Core as Core

{-------------------------------------------------------------------------------
  Transaction metadata
-------------------------------------------------------------------------------}

-- | Transaction metadata
--
-- NOTE: This does /not/ live in the acid-state database (and consequently
-- does not need a 'SafeCopy' instance), because this will grow without bound.
data TxMeta = TxMeta {
      -- | Transaction ID
      _txMetaId         :: Core.TxId

      -- | Total amount
      --
      -- TODO: What does this mean?
    , _txMetaAmount     :: Core.Coin

      -- | Transaction inputs
    , _txMetaInputs     :: NonEmpty (Core.Address, Core.Coin)

      -- | Transaction outputs
    , _txMetaOutputs    :: NonEmpty (Core.Address, Core.Coin)

      -- | Transaction creation time
    , _txMetaCreationAt :: Core.Timestamp

      -- | Is this a local transaction?
      --
      -- A transaction is local when /all/ of its inputs and outputs are
      -- to and from addresses owned by this wallet.
    , _txMetaIsLocal    :: Bool

      -- | Is this an outgoing transaction?
      --
      -- A transaction is outgoing when it decreases the wallet's balance.
    , _txMetaIsOutgoing :: Bool
    }

makeLenses ''TxMeta
