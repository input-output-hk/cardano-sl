module Cardano.Wallet.Kernel.DB.Compression where

import           Universum

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.SafeCopy as SC
import           Data.Set (Set)
import qualified Data.Set as S

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending (..))

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

type PendingDiff = InDb (Map Core.TxId Core.TxAux, Set Core.TxId)
type UtxoDiff = (Core.Utxo, Set Core.TxIn)
type BlockMetaDiff = (BlockMetaSlotIdDiff, BlockMetaAddressDiff)
type BlockMetaSlotIdDiff = InDb (Map Core.TxId Core.SlotId, Set Core.TxId)
type BlockMetaAddressDiff = (Map (InDb Core.Address) AddressMeta, Set (InDb Core.Address))

deltaPending :: Pending -> Pending -> PendingDiff
deltaPending (Pending inm) (Pending inm') = deltaM <$> inm <*> inm'

stepPending :: Pending -> PendingDiff -> Pending
stepPending (Pending inp) ind = Pending $ stepM <$> inp <*> ind

deltaUtxo :: InDb Core.Utxo -> InDb Core.Utxo -> InDb UtxoDiff
deltaUtxo inm inm' = deltaM <$> inm <*> inm'

stepUtxo :: InDb Core.Utxo -> InDb UtxoDiff -> InDb Core.Utxo
stepUtxo inu dinu = stepM <$> inu <*> dinu

deltaBlockMeta :: BlockMeta -> BlockMeta -> BlockMetaDiff
deltaBlockMeta (BlockMeta bmsi bma) (BlockMeta bmsi' bma') =
  (deltaM <$> bmsi <*> bmsi', deltaM bma bma')

stepBlockMeta :: BlockMeta -> BlockMetaDiff -> BlockMeta
stepBlockMeta (BlockMeta bmsi bms) (dbmsi, dbms) =
  BlockMeta (stepM <$> bmsi <*> dbmsi) (stepM bms dbms)

-- As a diff of two Maps we use the Map of new values (changed or completely new)
-- plus a Set of deleted values.
-- property: keys of the return set cannot be keys of the returned Map.
deltaM :: (Eq v, Ord k) => Map k v -> Map k v -> (Map k v, Set k)
deltaM newMap oldMap =
  let f a b = if a == b then Nothing else Just a
      newPairs = M.differenceWith f newMap oldMap -- this includes pairs that changed values.
      deletedKeys = M.keysSet $ M.difference oldMap newMap
  in (newPairs, deletedKeys)

-- @newPairs@ kai @deletedKeys@ should not have keys in common.
stepM :: Ord k => Map k v -> (Map k v, Set k) -> Map k v
stepM oldMap (newPairs, deletedKeys) =
  M.union newPairs lighterMap -- for common keys, union prefers the newPairs values.
    where lighterMap = M.withoutKeys oldMap deletedKeys

deltaS :: Ord k => Set k -> Set k -> (Set k, Set k)
deltaS newSet oldSet = (S.difference newSet oldSet, S.difference oldSet newSet)

stepS :: Ord k => Set k -> (Set k, Set k) -> Set k
stepS oldSet (new, deleted) = S.difference (S.union oldSet new) deleted

SC.deriveSafeCopy 1 'SC.base ''DeltaCheckpoint
