module Cardano.Wallet.Kernel.DB.Compression where

import           Universum

import qualified Data.SafeCopy as SC

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec.Pending (PendingDiff)
import           Cardano.Wallet.Kernel.Util

import           Test.Pos.Core.Arbitrary ()

-- This module aims to help define memory efficient Safecopy instances for Checkpoints.
-- This is done by defining new types, which are the diffs of the Checkpoints.
-- To achieve this we first define delta types for the building blocks of Checkpoint:
-- Map, Pending, BlockMeta and eventually Checkpoint.
--
-- To reduce boilerplate we could define a class like following:
-- class Differentiable A B | A -> B where
--   delta :: A -> A -> B
--   step  :: A -> B -> A

-- | This is the Delta type for both Checkpoint and PartialCheckpoint
data DeltaCheckpoint = DeltaCheckpoint {
    dcUtxo        :: !(InDb UtxoDiff)
  , dcUtxoBalance :: !(InDb Core.Coin)
  , dcPending     :: !PendingDiff
  , dcBlockMeta   :: !BlockMetaDiff
  , dcForeign     :: !PendingDiff
  , dcContext     :: !(Maybe BlockContext)
}

type UtxoDiff = MapDiff Core.TxIn Core.TxOutAux
type BlockMetaDiff = (BlockMetaSlotIdDiff, BlockMetaAddressDiff)
type BlockMetaSlotIdDiff = InDb (MapDiff Core.TxId Core.SlotId)
type BlockMetaAddressDiff = MapDiff (InDb Core.Address) AddressMeta

deltaUtxo :: InDb Core.Utxo -> InDb Core.Utxo -> InDb UtxoDiff
deltaUtxo inm inm' = deltaMap <$> inm <*> inm'

stepUtxo :: InDb Core.Utxo -> InDb UtxoDiff -> InDb Core.Utxo
stepUtxo inu dinu = stepMap <$> inu <*> dinu

deltaBlockMeta :: BlockMeta -> BlockMeta -> BlockMetaDiff
deltaBlockMeta (BlockMeta bmsi bma) (BlockMeta bmsi' bma') =
  (deltaMap <$> bmsi <*> bmsi', deltaMap bma bma')

stepBlockMeta :: BlockMeta -> BlockMetaDiff -> BlockMeta
stepBlockMeta (BlockMeta bmsi bms) (dbmsi, dbms) =
  BlockMeta (stepMap <$> bmsi <*> dbmsi) (stepMap bms dbms)

SC.deriveSafeCopy 1 'SC.base ''DeltaCheckpoint
