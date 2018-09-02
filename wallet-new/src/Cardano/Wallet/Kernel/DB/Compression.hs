module Cardano.Wallet.Kernel.DB.Compression where

import           Universum

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.SafeCopy as SC

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending, PendingDiff,
                     liftDeltaPending, liftStepPending)
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

deltaPending :: Pending -> Pending -> PendingDiff
deltaPending = liftDeltaPending deltaMap

stepPending :: Pending -> PendingDiff -> Pending
stepPending = liftStepPending stepMap

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

-- As a diff of two Maps we use the Map of new values (changed or completely new)
-- plus a Set of deleted values.
-- property: keys of the return set cannot be keys of the returned Map.
deltaMap :: (Eq v, Ord k) => Map k v -> Map k v -> MapDiff k v
deltaMap newMap oldMap =
  let f newEntry oldEntry = if newEntry == oldEntry then Nothing else Just newEntry
      newEntries = M.differenceWith f newMap oldMap -- this includes pairs that changed values.
      deletedKeys = M.keysSet $ M.difference oldMap newMap
  in MapDiff newEntries deletedKeys

-- newEntries should have no keys in common with deletedKeys.
stepMap :: Ord k => Map k v -> MapDiff k v -> Map k v
stepMap oldMap (MapDiff newEntries deletedKeys) =
  M.union newEntries lighterMap -- for common keys, union prefers the newPairs values.
    where lighterMap = M.withoutKeys oldMap deletedKeys

SC.deriveSafeCopy 1 'SC.base ''DeltaCheckpoint
