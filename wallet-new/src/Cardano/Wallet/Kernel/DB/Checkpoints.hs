module Cardano.Wallet.Kernel.DB.Checkpoints where

import           Universum

import qualified Cardano.Wallet.Kernel.Util.StrictList as SL
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.SafeCopy as SC
import           Data.Set (Set)
import qualified Data.Set as S
import           Test.QuickCheck (Arbitrary (..), arbitrary)

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import           Pos.Core.Chrono (NewestFirst (..))

import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending (..))
import           Cardano.Wallet.Kernel.Util.StrictNonEmpty (StrictNonEmpty (..))
import qualified Cardano.Wallet.Kernel.Util.StrictNonEmpty as SNE

import           Test.Pos.Core.Arbitrary ()

newtype Checkpoints = Checkpoints {unCheckpoints :: NewestFirst StrictNonEmpty Checkpoint}
  deriving (Eq, Show)

take :: Int -> Checkpoints -> Checkpoints
take n (Checkpoints (NewestFirst ls)) = Checkpoints (NewestFirst (SNE.take n ls))

instance Arbitrary Checkpoints where
  arbitrary = do
    ls <- arbitrary
    pure . Checkpoints . NewestFirst $ ls

instance SC.SafeCopy Checkpoints where
    getCopy = SC.contain $ do
      c <- SC.safeGet
      ds <- SC.safeGet
      pure $ steps (c,ds)
    putCopy cs  = SC.contain $ do
      let (c,ds) = deltas cs
      SC.safePut c
      SC.safePut ds

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

deltas :: Checkpoints -> (Checkpoint, [DeltaCheckpoint])
deltas (Checkpoints (NewestFirst (a SNE.:| strict))) = (a, go a strict)
  where
    go :: Checkpoint -> SL.StrictList Checkpoint -> [DeltaCheckpoint]
    go _ SL.Nil                  = []
    go c (SL.Cons c' strictRest) = (deltaC c' c) : (go c' strictRest)

steps :: (Checkpoint, [DeltaCheckpoint] ) -> Checkpoints
steps (c, ls) = Checkpoints . NewestFirst $ c SNE.:| go c ls
  where
    go :: Checkpoint -> [DeltaCheckpoint] -> SL.StrictList Checkpoint
    go _ []         = SL.Nil
    go c' (dc:rest) = let new = stepC c' dc in SL.Cons new (go new rest)

deltaC :: Checkpoint -> Checkpoint -> DeltaCheckpoint
deltaC c c' = DeltaCheckpoint {
    dcUtxo         = deltaUtxo (_checkpointUtxo c) (_checkpointUtxo c')
  , dcUtxoBalance  = _checkpointUtxoBalance c
  , dcPending      = deltaPending (_checkpointPending c) (_checkpointPending c')
  , dcBlockMeta    = deltaBlockMeta (_checkpointBlockMeta c) (_checkpointBlockMeta c')
  , dcForeign      = deltaPending (_checkpointForeign c) (_checkpointForeign c')
  , dcContext      = _checkpointContext c
}

stepC :: Checkpoint -> DeltaCheckpoint -> Checkpoint
stepC c DeltaCheckpoint{..} =
  Checkpoint {
    _checkpointUtxo          = stepUtxo (_checkpointUtxo c) dcUtxo
    , _checkpointUtxoBalance = dcUtxoBalance
    , _checkpointPending     = stepPending (_checkpointPending c) dcPending
    , _checkpointBlockMeta   = stepBlockMeta (_checkpointBlockMeta c) dcBlockMeta
    , _checkpointContext     = dcContext
    , _checkpointForeign     = stepPending (_checkpointForeign c) dcForeign
  }

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
